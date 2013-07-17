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


(defn- concrete-path-step [obj path-step]
  (if (and (vector? obj) (not (number? path-step))) ; assert maybe
    (let [[k v] (first path-step)
          result (first (keep-indexed (fn [i item] (when (= (get item k) v) i)) obj))]
      (if-not result
        (throw (IllegalArgumentException. (str "Could not find a match for '" path-step "'.")))
        result))
    #_else
    path-step))


;;; General functions, used by the other concrete path operations.

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


(defn- relative-vector-index [length i]
  (if (>= i length)
    (dec length)
    #_else
    (let [i (if (< i 0) (+ i length) #_else i)]
      (if (< i 0) 0  #_else i))))


;;; Concrete opertion, append to.

(defn append-to-vo
  "Given a path in a VO that points to an array, this append the value
  at the end of that array."
  [vo path value]
  (update-in-vo vo path conj value))


;;; Concrete opertion, insert at.

(defn- vector-insert-at [vec index value]
  {:pre [vector? vec]}
  (let [index (relative-vector-index (count vec) index)]
    (apply conj (subvec vec 0 index) value (subvec vec index))))


(defn insert-at
  "Given a path in a VO that points an index in an array, this inserts
  the value at that specific index."
  [vo path value]
  {:pre [(integer? (last path))]}
  (let [index (last path)
        path (pop path)]
    (update-in-vo vo path vector-insert-at index value)))


;;; Concrete opertion, move to.

(defn- vector-remove [vec i]
  {:pre [vector? vec]}
  (cond (empty? vec) []
        (>= i (dec (count vec))) (subvec vec 0 (- (count vec) 1))
        (<= i 0) (subvec vec 1)
        :else (apply conj (subvec vec 0 i) (subvec vec (inc i)))))


(defn- vector-move-item [vec from to]
  {:pre [vector? vec]}
  (let [from (concrete-path-step vec from)
        from (relative-vector-index (count vec) from)
        to (relative-vector-index (count vec) to)]
    (if (= from to) vec
        #_else
        (vector-insert-at (vector-remove vec from) to (nth vec from)))))


(defn move-vo-to
  "Given a path in a VO to an value in an array, this moves that value
  to the specified index in that array."
  [vo path to]
  (let [from (last path)
        path (pop path)]
    (update-in-vo vo path vector-move-item from to)))


;;; Concrete operation, replace at.

(defn- update-with-replace
  [container at value]
  (if (vector? container)
    (assoc container (relative-vector-index (count container) at) value)
    (assoc container at value)))


(defn replace-at
  "Given a path in a VO, this replaces the value pointed to by that
  path by the specified new value. The path can point to a concrete
  value or to an array. In case it points to an array, that entire
  array is being replaced."
  [vo path value]
  (let [at (last path)
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
  (let [at (last path)
        path (pop  path)]
    (update-in-vo vo path remove-at at)))
