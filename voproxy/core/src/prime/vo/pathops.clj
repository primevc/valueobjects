;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.pathops
  "Operations (add, update, ...) for ValueObject using value-paths."
  (:use prime.utils))


;;; Path helper functions.

(defn- fill-path-step [step var]
  (if (map? step)
    (let [[k v] (first step)]
      (if-not (nil? v) step #_else {k var}))
    #_else
    (or-not nil? step var)))


(defn fill-path
  "Takes a sequence of path steps and a sequence of variables, where
  each nil path step is replaced by the corresponding variable. For
  example: (fill-path [1 nil nil 4] [2 3]) => (1 2 3 4)"
  [path variables]
  (let [[step & path-rest] path
        path-value (fill-path-step step (first variables))
        variables (if (= path-value step) variables #_else (next variables))]
    (cons path-value
          (when-not (empty? path-rest)
            (fill-path path-rest variables)))))


(defn array-like?
  [val]
  (and val (or (sequential? val) (.. (class val) isArray) (instance? java.util.List val))))


(defn concrete-path-step [obj path-step]
  (if (and (array-like? obj) (not (number? path-step)))
    (let [[k v] (first path-step)
          result (first (keep-indexed (fn [i item] (when (= (get item k) v) i)) obj))]
      (if-not result
        (throw (IllegalArgumentException. (str "Could not find a match for '" path-step "'.")))
        result))
    #_else
    path-step))


;;; General functions, used by the other concrete path operations.

(defn get-in-vo
  "Returns the value in the VO following the path steps. The added
  value of this function over the core `get-in` function is that this
  function understands our own type of paths."
  [m [k & ks]]
  (let [k (concrete-path-step m k)]
    (if ks
      (recur (get m k) ks)
      (get m k))))


(defn- update-in-vo
  "'Updates' a value in a nested ValueObject, where ks is a sequence of
  keys and f is a function that will take the old value and any
  supplied args and return the new value, and returns a new
  ValueObject. The added value of this function over the core
  update-in is it understands our own type of paths. It also supports
  empty paths, unlike the core update-in."
  [m [k & ks] f & args]
  (let [k (concrete-path-step m k)]
    (if ks
      (assoc m k (apply update-in-vo (get m k) ks f args))
      (if k
        (assoc m k (apply f (get m k) args))
        (apply f m args)))))


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


;;; Concrete opertion, append to.

(defn append-to-vo
  "Given a path in a VO that points to an array, this append the value
  at the end of that array."
  [vo path value]
  (if (array-like? value)
    (update-in-vo vo path concat value)
    (update-in-vo vo path conj value)))


;;; Concrete opertion, insert at.

(defn- vector-insert-at [vec index value & options]
  {:pre [(vector? vec)]}
  (let [index (apply relative-vector-index (count vec) index options)]
    ;; ---TODO: Fix this, as (apply vector ...) is now needed since working on ScalaSeqWrapper
    ;;          fails to conj.
    (apply conj (apply vector (subvec vec 0 index)) value (subvec vec index))))


(defn insert-at
  "Given a path in a VO that points to an index in an array, this inserts
  the value at that specific index."
  [vo path value]
  {:pre [(integer? (last path))]}
  (let [path (vec path)
        index (last path)
        path (pop path)]
    (update-in-vo vo path vector-insert-at index value)))


;;; Concrete opertion, move to.

(defn- vector-remove [vec i]
  {:pre [(vector? vec)]}
  (cond (empty? vec) []
        (>= i (dec (count vec))) (subvec vec 0 (- (count vec) 1))
        (<= i 0) (subvec vec 1)
        :else (apply conj (subvec vec 0 i) (subvec vec (inc i)))))


(defn- vector-move-item [vec from to]
  {:pre [(array-like? vec) (not (empty? vec))]}
  (let [vec (clojure.core/vec vec) ; Hack: subvec behaves different on Scala vectors vs Clojure's.
        from (relative-vector-index (count vec) (concrete-path-step vec from))
        to (relative-vector-index (count vec) to :allow-index-after-last)
        to-after-remove (if (< from to) (dec to) to)]
    (if (= from to) vec
        (vector-insert-at (vector-remove vec from) to-after-remove
                          (nth vec from) :allow-index-after-last))))


(defn move-vo-to
  "Given a path in a VO to an value in an array, this moves that value
  to the specified index in that array."
  [vo path to]
  (let [path (vec path)
        from (last path)
        path (pop path)]
    (update-in-vo vo path vector-move-item from to)))


;;; Concrete operation, replace at.

(defn- update-with-replace
  [container at value]
  (assert container "replace is only possible in existing container")
  (if (vector? container)
    (let [index (relative-vector-index (count container) at)]
      (if (array-like? value)
        (let [[pre post] (split-at index container)]
          (into [] (concat pre value (next post))))
        (assoc container index value)))
    (assoc container at value)))


(defn replace-at
  "Given a path in a VO, this replaces the value pointed to by that
  path by the specified new value. The path can point to a concrete
  value or to an array. In case it points to an array, that entire
  array is being replaced."
  [vo path value]
  (let [path (vec path)
        at (last path)
        path (pop path)]
    (update-in-vo vo path update-with-replace at value)))


;;; Concrete opertion, merge at.

(defn merge-at
  "Given a path in a VO, pointing to a VO, this merges the given map
  into that VO, replacing any existing values."
  [vo path value]
  (update-in-vo vo path merge value))


;;; Concrete opertion, remove from.

(defn- find-index [vec obj]
  {:pre [(vector? vec)]}
  (first (keep-indexed (fn [i item] (if (= item obj) i)) vec)))


(defn- remove-at [map-or-vec key]
  (if (vector? map-or-vec)
    (if-let [key (if (number? key) key #_else (find-index map-or-vec key))]
      (vector-remove map-or-vec (relative-vector-index (count map-or-vec) key))
      #_else
      map-or-vec)
    #_else
    (dissoc map-or-vec key)))


(defn remove-from
  "Given a path in a VO, removes that what it points to."
  [vo path]
  (let [path (vec path)
        at (last path)
        path (pop path)]
    (update-in-vo vo path remove-at at)))
