;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.pathops
  "Operations (add, update, ...) for ValueObject using value-paths.")

(defn fill-path-step [step var]
  (if (map? step)
    (let [[k v] (first step)]
      (if v step #_else {k var}))
  #_else
    (or step var)))

(defn fill-path [path variables]
  (let [[step & path-rest] path
         path-value        (fill-path-step step (first variables))
         variables         (if (= path-value step) variables #_else (next variables))]
    (cons path-value
          (if (not (empty? path-rest))
            (fill-path path-rest variables)))))

#_(fill-path [:booklet :spreads {:id 12} nil nil :id nil] [1 2 3 4])

(defn concrete-path-step [obj path-step]
  (if (and (vector? obj) (not (number? path-step))) ; assert maybe
    (let [[k v] (first path-step)]
      (first (keep-indexed (fn [i item] (if (= (get item k) v) i)) obj)))
  #_else
    path-step))

(concrete-path-step [{} {} {:id 1 :tags "ahoi"}] {:id 1})


(defn get-or-find [obj key]
  (if (and (vector? obj) (map? key)) ; assert maybe
    (let [[k v] (first key)]
      (first (filter #(= (get % k) v) obj)))
  #_else
    (get obj key)))

;(get-or-find {:id 1 :tags "ahoi"} :tags)

;
; appendTo
;

(defn update-in-vo
  "'Updates' a value in a nested ValueObject, where ks is a
  sequence of keys and f is a function that will take the old value
  and any supplied args and return the new value, and returns a new
  ValueObject."
  {:added "1.0"
   :static true}
  ([m [k & ks] f & args]
   (let [k (concrete-path-step m k)]
     (if ks
       (assoc m k (apply update-in-vo (get m k) ks f args))
       (assoc m k (apply f (get m k) args))))))

(defn append-to-vo [vo path value]
  (update-in-vo vo path conj value))

(append-to-vo {:booklet {:spreads [{:id 1 :tags ["ahoi"]}]}}
           [:booklet :spreads {:id 1} :tags]
           "string")


;
; insertAt
;

(defn relative-vector-index [length i]
  (if (>= i length)
    (dec length)
  #_else
    (let [i (if (< i 0) (+ i length) #_else i)]
      (if (< i 0) 0  #_else i))))


(defn vector-insert-at [vec index value]
  {:pre [vector? vec]}
  (let [length (count vec)]
    (if (or (= -1 index) (>= index length))
      (conj vec value) ; append
    #_else
      (let [index (relative-vector-index length index)]
        (apply conj
         (subvec vec 0 index)
         value
         (subvec vec index))))))

(vector-insert-at [1 2 3] 0 :a)
(vector-insert-at [1 2 3] 1 :a)
(vector-insert-at [1 2 3] 2 :a)
(vector-insert-at [1 2 3] 3 :a)


(defn insert-at [vo path value]
  {:pre [(integer? (last path))]}
  (let [index (last path)
        path  (pop path)]
    (update-in-vo vo path vector-insert-at index value)))

(insert-at {:booklet {:spreads [{:id 1 :tags [:a :b :c "ahoi"]}]}}
           [:booklet :spreads {:id 1} :tags 4]
           "string")


;
; moveTo
;

(defn vector-remove [vec i]
  {:pre [vector? vec]}
  (cond
   (empty? vec)       []
   (>= i (dec (count vec))) (subvec vec 0 (- (count vec) 1))
   (<= i 0)                 (subvec vec 1)
   :else                    (apply conj (subvec vec 0 i) (subvec vec (inc i)))
  ))


(defn vector-move-item [vec from to]
  {:pre [vector? vec]}
  (let [from (relative-vector-index (count vec) from)
        to   (relative-vector-index (count vec) to)]
    (if (= from to) vec
    #_else
      (vector-insert-at (vector-remove vec from) to (nth vec from)))))

(vector-move-item [0 1 2 3 4 5 6] -1 -1)


(defn move-vo-to [vo path to]
  (let [from (last path)
        path (pop  path)]
    (update-in-vo vo path vector-move-item from to)))

(move-vo-to {:booklet {:spreads [{:id 1 :tags ["ahoi" 0 1 2 3 4 5]}]}}
           [:booklet :spreads {:id 1} :tags 0]
           3)

;
; replaceAt
;

(defn vector-replace-at [vec at value]
  {:pre [vector? vec]}
  (assoc vec (relative-vector-index (count vec) at) value))


(defn replace-at [vo path value]
  (let [at   (last path)
        path (pop  path)]
    (update-in-vo vo path vector-replace-at at value)))

(replace-at {:booklet {:spreads [{:id 1 :tags ["ahoi" 0 1 2 3 4 5]}]}}
           [:booklet :spreads {:id 1} :tags -0]
           "VET!")

;
; mergeAt
;



(defn merge-at [vo path value]
  (update-in-vo vo path merge value))

(merge-at {:booklet {:spreads [{:id 1 :tags ["ahoi" 0 1 2 3 4 5]}]}}
           [:booklet :spreads {:id 1}]
           {:id 134})



;
; removeFrom
;
(defn find-index [vec obj]
  {:pre [(vector? vec)]}
  (first (keep-indexed (fn [i item] (if (= item obj) i)) vec)))

(defn remove-at [map-or-vec key]
  (if (vector? map-or-vec)
    (if-let [key (if (number? key) key #_else (find-index map-or-vec key))]
      (vector-remove map-or-vec (relative-vector-index (count map-or-vec) key))
    #_else
      map-or-vec)
  #_else
    (dissoc map-or-vec key)))


(defn remove-from [vo path]
  (let [at   (last path)
        path (pop  path)]
  (update-in-vo vo path remove-at at)))



(remove-from {:booklet {:spreads [{:id 1 :tags ["ahoi" 0 1 2 3 4 5]}]}}
           [:booklet :spreads {:id 1} :tags "ahoi"])






