;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.util.elasticsearch.script
  "This namespace contains the script functions used by update-doc. As
  it is unnecessary to transfer the entire script for each request, at
  the end of this file the script functions are read and combined to
  smaller scripts."
  (:import [java.util List Map LinkedHashMap ArrayList]))


;;;;
;;;; Script functions.
;;;;


;;; Helper methods.

(defmacro debug [& args] (when #_true false `(println ~@args)))


(defn to=
  [this that]
  (= this that))


(defn array-like?
  [val]
  (and val (or (sequential? val) (.. (class val) isArray) (instance? java.util.List val))))


(defn concrete-path-step
  [obj path-step]
  (if (and (array-like? obj) (not (number? path-step)))
    (let [[k v] (first path-step)
          result (first (keep-indexed
                         (if (instance? Map (first obj))
                           (fn [i item] (when (to= (get item k) v) i)) ; for {"vo-key" value} lookups
                           (if (#{:= "="} k)
                             (fn [i item] (when (to= item v) i))  ; for {"=" value"} literal lookups
                             (throw (IllegalArgumentException.
                                     "Must use {:= ...} map for primitive array lookups"))))
                         obj))]
      (if-not result
        (throw (IllegalArgumentException. (str "Could not find a match for " (pr-str path-step))))
        result))
    #_else
    path-step))


(defn relative-vector-index [length i & options]
  "Calculates the index relative to a vector length. Optionally, one
  can specify that an extra index at the end is allowed, using the
  :allow-index-after-last keyword. The relative index for a length of
  0 is always 0. Examples:

  (relative-vector-index 3 2)  ;=> 2
  (relative-vector-index 3 4)  ;=> 2
  (relative-vector-index 3 -1) ;=> 2
  (relative-vector-index 3 -3) ;=> 0
  (relative-vector-index 3 -4) ;=> 0
  (relative-vector-index 0 0)  ;=> 0
  (relative-vector-index 3 4 :allow-index-after-last)  ;=> 3
  (relative-vector-index 3 -1 :allow-index-after-last) ;=> 3
  (relative-vector-index 0 2 :allow-index-after-last)  ;=> 0"
  (if (= 0 length)
    0
    (let [options (zipmap options (repeat true))
          div (if (:allow-index-after-last options) (inc length) length)]
      (if (>= i div) (dec div) (mod (max i (- div)) div)))))


(defn- update-in-vo
  [obj path f & args]
  (debug "UPDATE IN VO" obj path f args)
  (if-let [key (and path (concrete-path-step obj (first path)))]
    (let [inner (and obj (if (instance? Map obj) (.get ^Map obj key) #_else (.get ^List obj key)))
          value (apply update-in-vo inner (next path) f args)]
      (debug "BACKTRACK VALUE" value "- WILL BE PUT IN" obj "USING KEY" key)
      (cond (and (number? key) (instance? Map inner))
            (do (doseq [[k v] value] (.put ^Map inner k v)) obj)
            (number? key)
            (let [^List obj (or obj (new ArrayList))] (.set obj key value) obj)
            :else
            (let [^Map obj (or obj (new LinkedHashMap))] (.put obj key value) obj)))
    (apply f obj args)))


;;; Operation implementations.

;; Moving

(defn- move-to*
  [^List obj from to]
  (debug "MOVE TO" obj from to)
  (let [from (->> from
                  (concrete-path-step obj)
                  (relative-vector-index (count obj)))
        to (relative-vector-index (count obj) to)
        val (.remove obj (int from))]
    (doto obj (.add (int to) val))))


(defn move-to
  [{:strs [path to ctx]}]
  (let [doc (get ctx "_source")]
    (update-in-vo doc (butlast path) move-to* (last path) to)))


;; Merging

(defn- merge-at*
  [^Map obj ^Map value]
  (debug "MERGE AT" obj value)
  (if obj
    (do (doseq [[k v] value] (.put obj k v))
        obj)
  ;else
    value))


(defn merge-at
  [{:strs [path value ctx]}]
  (let [doc (get ctx "_source")]
    (update-in-vo doc path merge-at* value)))


;; Replacing

(defn- replace-at*
  [obj at value]
  (debug "REPLACE AT" obj at value)
  (let [at (concrete-path-step obj at)
        at (if (and (array-like? obj) (number? at))
             (relative-vector-index (count obj) at)
             at)]
    (if (array-like? obj)
      (.remove ^List obj (int at))
      (.remove ^Map obj at))
    (cond (and (array-like? obj) (array-like? value))
          (.addAll ^List obj at value)
          (array-like? obj)
          (.add ^List obj at value)
          :else
          (.put ^Map obj at value)))
  obj)


(defn replace-at
  [{:strs [path value ctx]}]
  (let [doc (get ctx "_source")]
    (update-in-vo doc (butlast path) replace-at* (last path) value)))


;; Removing

(defn- remove-at*
  [obj at]
  (debug "REMOVE FROM" obj at)
  (let [at (concrete-path-step obj at)
        at (if (and (array-like? obj) (number? at))
             (relative-vector-index (count obj) at)
             at)]
    (if (array-like? obj)
      (.remove ^List obj (int at))
      (.remove ^Map obj at)))
  obj)


(defn remove-at
  [{:strs [path ctx]}]
  (let [doc (get ctx "_source")]
    (update-in-vo doc (butlast path) remove-at* (last path))))


;; Inserting

(defn- insert-at*
  [^List obj at value]
  (debug "INSERT AT" obj at value)
  (let [at (concrete-path-step obj at)
        obj (or obj (new ArrayList))
        at (relative-vector-index (count obj) at)]
    (if (array-like? value)
      (.addAll ^List obj at value)
      (.add ^List obj at value))
    obj))


(defn insert-at
  [{:strs [path value ctx]}]
  (let [doc (get ctx "_source")]
    (update-in-vo doc (butlast path) insert-at* (last path) value)))


;; Appending

(defn- append-to*
  [^List obj ^Map value]
  (debug "APPEND TO" obj value)
  (let [obj (or obj (new ArrayList))]
    (if (array-like? value)
      (.addAll ^List obj value)
      (.add ^List obj value))
    obj))


(defn append-to
  [{:strs [path value ctx]}]
  (let [doc (get ctx "_source")]
    (update-in-vo doc path append-to* value)))



;;;;
;;;; Script source functions.
;;;;
;;;; Each script must end with an function that takes an `env` as its sole parameter.
;;;;


;;; Functions to read the source of namespace declarations and functions.

(require '[clojure.java.io :as io]
         '[clojure.string :as string])


(defn- read-ns
  "Reads the (.clj) source file of a namespace (string or symbol).
  Returns a sequence of the forms within that source file. The source
  file should be on the classpath."
  [ns]
  (binding [*read-eval* false]
    (-> ns
        str
        (string/replace "." "/")
        (string/replace "-" "_")
        (str ".clj")
        io/resource
        io/reader
        slurp
        (str "]")
        (->> (str "["))
        read-string)))


(defmacro ns-source
  "Reads the source file of the given namespace (string or symbol),
  and returns the String representation of the first form. This is
  probably the (ns ...) form. Sophisticated, isn't it?!"
  [ns]
  (binding [*print-meta* true]
    (pr-str (first (read-ns ns)))))


(defmacro def-source
  "Reads the source file of a given namespace-qualified symbol, and
  returns the last def, defn, defn- or defmacro form that has the
  symbol as the second part of that symbol. That means, no metadata
  support yet. Again, very sophisticated!"
  [sym]
  (let [source (read-ns (namespace sym))]
    (binding [*print-meta* true]
      (pr-str (last (filter (fn [form]
                              (and (#{'def 'defn 'defn- 'defmacro} (first form))
                                   (= (name sym) (str (second form))))) source))))))


;;; Per operation script source strings.

(def base-script
  (str (ns-source prime.vo.util.elasticsearch.script)
       (def-source prime.vo.util.elasticsearch.script/debug)
       (def-source prime.vo.util.elasticsearch.script/to=)
       (def-source prime.vo.util.elasticsearch.script/array-like?)
       (def-source prime.vo.util.elasticsearch.script/concrete-path-step)
       (def-source prime.vo.util.elasticsearch.script/relative-vector-index)
       (def-source prime.vo.util.elasticsearch.script/update-in-vo)))

(def move-to-script
  (str base-script
       (def-source prime.vo.util.elasticsearch.script/move-to*)
       (def-source prime.vo.util.elasticsearch.script/move-to)))

(def merge-at-script
  (str base-script
       (def-source prime.vo.util.elasticsearch.script/merge-at*)
       (def-source prime.vo.util.elasticsearch.script/merge-at)))

(def replace-at-script
  (str base-script
       (def-source prime.vo.util.elasticsearch.script/replace-at*)
       (def-source prime.vo.util.elasticsearch.script/replace-at)))

(def remove-at-script
  (str base-script
       (def-source prime.vo.util.elasticsearch.script/remove-at*)
       (def-source prime.vo.util.elasticsearch.script/remove-at)))

(def insert-at-script
  (str base-script
       (def-source prime.vo.util.elasticsearch.script/insert-at*)
       (def-source prime.vo.util.elasticsearch.script/insert-at)))

(def append-to-script
  (str base-script
       (def-source prime.vo.util.elasticsearch.script/append-to*)
       (def-source prime.vo.util.elasticsearch.script/append-to)))
