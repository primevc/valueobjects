;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.util.elasticsearch.script
  (:require [prime.vo.pathops :as po])
  (:import [java.util List Map LinkedHashMap ArrayList]))


;;;;
;;;; Script functions.
;;;;


;;; Helper methods.

(defmacro debug
  [& args]
  ;; (println ~@args)
  )


(defn- update-in-vo
  [obj path f & args]
  (debug "UPDATE IN VO" obj path f args)
  (if-let [key (and path (po/concrete-path-step obj (first path)))]
    (let [inner (and obj (.get obj key))
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
  [obj from to]
  (debug "MOVE TO" obj from to)
  (let [from (po/relative-vector-index (count obj) from)
        to (po/relative-vector-index (count obj) to :allow-index-after-last)
        to (if (< from to) (dec to) to)
        val (.remove ^List obj (int from))]
    (doto obj (.add (int to) val))))


(defn move-to
  [{:strs [path to ctx]}]
  (let [doc (get ctx "_source")]
    (update-in-vo doc (butlast path) move-to* (last path) to)))


;; Merging

(defn- merge-at*
  [^Map obj ^Map value]
  (debug "MERGE AT" obj value)
  (doseq [[k v] value]
    (.put obj k v))
  obj)


(defn merge-at
  [{:strs [path value ctx]}]
  (let [doc (get ctx "_source")]
    (update-in-vo doc path merge-at* value)))


;; Replacing

(defn- replace-at*
  [obj at value]
  (debug "REPLACE AT" obj at value)
  (let [at (if (and (po/array-like? obj) (number? at))
             (po/relative-vector-index (count obj) at)
             at)]
    (if (po/array-like? obj)
      (.remove ^List obj (int at))
      (.remove ^Map obj at))
    (cond (and (po/array-like? obj) (po/array-like? value))
          (.addAll ^List obj at value)
          (po/array-like? obj)
          (.add ^List obj at value)
          :else
          (.put ^Map obj at value)))
  obj)


(defn replace-at
  [{:strs [path value ctx]}]
  (let [doc (get ctx "_source")]
    (update-in-vo doc (butlast path) replace-at* (last path) value)))


;; Removing

(defn- remove-from*
  [obj at]
  (debug "REMOVE FROM" obj at)
  (let [at (if (and (po/array-like? obj) (number? at))
             (po/relative-vector-index (count obj) at)
             at)]
    (if (po/array-like? obj)
      (.remove ^List obj (int at))
      (.remove ^Map obj at)))
  obj)


(defn remove-from
  [{:strs [path ctx]}]
  (let [doc (get ctx "_source")]
    (update-in-vo doc (butlast path) remove-from* (last path))))


;; Inserting

(defn- insert-at*
  [^List obj at value]
  (debug "INSERT AT" obj at value)
  (let [obj (or obj (new ArrayList))
        at (po/relative-vector-index (count obj) at)]
    (if (po/array-like? value)
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
    (if (po/array-like? value)
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

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as string])


(defn- read-ns
  "Reads the (.clj) source file of a namespace (string or symbol).
  Returns a sequence of the forms within that source file. The source
  file should be on the classpath."
  [ns]
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
      edn/read-string))


(defn ns-source
  "Reads the source file of the given namespace (string or symbol),
  and returns the String representation of the first form. This is
  probably the (ns ...) form. Sophisticated, isn't it?!"
  [ns]
  (binding [*print-meta* true]
    (pr-str (first (read-ns ns)))))


(defn def-source
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


(def base-script
  (str (ns-source 'prime.vo.pathops)
       (def-source 'prime.vo.pathops/array-like?)
       (def-source 'prime.vo.pathops/concrete-path-step)
       (def-source 'prime.vo.pathops/relative-vector-index)
       (ns-source 'prime.vo.util.elasticsearch.script)
       (def-source 'prime.vo.util.elasticsearch.script/debug)
       (def-source 'prime.vo.util.elasticsearch.script/update-in-vo)))

(def move-to-script
  (str base-script
       (ns-source 'prime.vo.util.elasticsearch.script)
       (def-source 'prime.vo.util.elasticsearch.script/move-to*)
       (def-source 'prime.vo.util.elasticsearch.script/move-to)))

(def merge-at-script
  (str base-script
       (ns-source 'prime.vo.util.elasticsearch.script)
       (def-source 'prime.vo.util.elasticsearch.script/merge-at*)
       (def-source 'prime.vo.util.elasticsearch.script/merge-at)))

(def replace-at-script
  (str base-script
       (ns-source 'prime.vo.util.elasticsearch.script)
       (def-source 'prime.vo.util.elasticsearch.script/replace-at*)
       (def-source 'prime.vo.util.elasticsearch.script/replace-at)))

(def remove-from-script
  (str base-script
       (ns-source 'prime.vo.util.elasticsearch.script)
       (def-source 'prime.vo.util.elasticsearch.script/remove-from*)
       (def-source 'prime.vo.util.elasticsearch.script/remove-from)))

(def insert-at-script
  (str base-script
       (ns-source 'prime.vo.util.elasticsearch.script)
       (def-source 'prime.vo.util.elasticsearch.script/insert-at*)
       (def-source 'prime.vo.util.elasticsearch.script/insert-at)))

(def append-to-script
  (str base-script
       (ns-source 'prime.vo.util.elasticsearch.script)
       (def-source 'prime.vo.util.elasticsearch.script/append-to*)
       (def-source 'prime.vo.util.elasticsearch.script/append-to)))
