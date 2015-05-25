;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.proxy.memory
  "An implementation of the VOProxy protocol, keeping all data in
  memory. This implementation is mostly suitable for unit testing or
  interactive development."
  (:require [clojure.string :refer (split)]
            [prime.utils :as utils]
            [prime.vo :refer (manifest)]
            [prime.vo.proxy :refer :all]
            [prime.vo.pathops :as pathops]
            [taoensso.timbre :refer (trace debug info warn error fatal spy)]))


;;; Helper functions.

(defn- votype->hex
  "Evaluates to a HEX String representation of the type of the supplied
  VO."
  [vo]
  (Integer/toHexString (.ID (manifest vo))))

(defn- assert-not-nil-id [vo]
  (assert (not (nil? (:id vo))) (print-str "id required, but" (:id vo) "id supplied for vo:" vo)))

(defn- matches?
  "Tests whether the search VO matches the actual VO."
  [search actual]
  (letfn [(vector-merge [actual search]
            (if (and (vector? search)
                     (every? #(some (partial matches? %) actual) search))
              actual
              search))]
    (= actual (utils/deep-merge-with vector-merge actual search))))

;;; Simuation of elasticsearch filters

(defn- es-filter
  "Supports:

    range filter -  with `lt`, `lte`, `gt` and `gte`."
  [options vos]
  (if-let [paths (get-in options [:filter "range"])]
    (filter (fn [vo]
              (loop [paths paths]
                (if-let [[path tests] (first paths)]
                  (let [fields (-> (name path) (split #"\.") (->> (map keyword)))
                        value (reduce get vo fields)
                        matches? (every? (fn [[test-type test-value]]
                                           (case test-type
                                             "lt" (< (compare value test-value) 0)
                                             "lte" (<= (compare value test-value) 0)
                                             "gt" (> (compare value test-value) 0)
                                             "gte" (>= (compare value test-value) 0)))
                                         tests)]
                    (if matches?
                      (recur (next paths))
                      false))
                  true)))
            vos)
    vos))

;;; Define the in-memory proxy, both implementing VOProxy as well as
;;; VOSearchProxy. The MemoryVOProxy takes an atom encapsulating a map as its
;;; sole argument.

(deftype MemoryVOProxy [data]
  VOProxy
  (get-vo [this vo]
    (assert-not-nil-id vo)
    (debug "Getting VO based on" vo)
    (get-in @data [(votype->hex vo) (:id vo)]))

  (put-vo [this vo]
    (put-vo this vo {}))

  (put-vo [this vo options]
    (assert-not-nil-id vo)
    (debug "Putting VO" vo)
    (swap! data assoc-in [(votype->hex vo) (:id vo)] vo))

  (update [this vo]
    (update this vo (:id vo) {}))

  (update [this vo id]
    (update this vo (or id (:id vo)) {}))

  (update [this vo id options]
    (let [id (or id (:id vo))]
      (assert id (print-str "id required, but no id supplied for vo:" vo))
      (debug "Updating VO" vo "having ID" id "with options" options)
      (swap! data update-in [(votype->hex vo) id]
             (partial utils/deep-merge-with (fn [x y] y))
             (assoc vo :id id))))

  (delete [this vo]
    (delete this vo {}))

  (delete [this vo options]
    (assert-not-nil-id vo)
    (debug "Deleting VO based on" vo "with options" options)
    (swap! data update-in [(votype->hex vo)] dissoc (:id vo)))

  (append-to [this vo path path-vars value]
    (append-to this vo path path-vars value {}))

  (append-to [this vo path path-vars value options]
    (assert-not-nil-id vo)
    (debug "Appending" value "identified by" vo " on path" path "with vars" path-vars
           "having options" options)
    (let [filled-path (pathops/fill-path path path-vars)
          updated-vo (pathops/append-to-vo (get-vo this vo) filled-path value)]
      (put-vo this updated-vo (:id updated-vo))))

  (insert-at [this vo path path-vars value]
    (insert-at this vo path path-vars value {}))

  (insert-at [this vo path path-vars value options]
    (assert-not-nil-id vo)
    (debug "Inserting" value "identified by" vo " on path" path "with vars" path-vars
           "having options" options)
    (let [filled-path (pathops/fill-path path path-vars)
          updated-vo (pathops/insert-at (get-vo this vo) filled-path value)]
      (put-vo this updated-vo (:id updated-vo))))

  (move-to [this vo path path-vars to]
    (move-to this vo path path-vars to {}))

  (move-to [this vo path path-vars to options]
    (assert-not-nil-id vo)
    (debug "Moving value identified by" vo "on path" path "with vars" path-vars
           "having options" options "to" to)
    (let [filled-path (pathops/fill-path path path-vars)
          updated-vo (pathops/move-vo-to (get-vo this vo) filled-path to)]
      (put-vo this updated-vo (:id updated-vo))))

  (replace-at [this vo path path-vars value]
    (replace-at this vo path path-vars value {}))

  (replace-at [this vo path path-vars value options]
    (assert-not-nil-id vo)
    (debug "Replacing value identified by" vo "on path" path "with vars" path-vars
           "having options" options "with" value)
    (let [filled-path (pathops/fill-path path path-vars)
          updated-vo (pathops/replace-at (get-vo this vo) filled-path value)]
      (put-vo this updated-vo (:id updated-vo))))

  (merge-at [this vo path path-vars value]
    (merge-at this vo path path-vars value {}))

  (merge-at [this vo path path-vars value options]
    (assert-not-nil-id vo)
    (debug "Merging" value "identified by" vo "on path" path "with vars" path-vars
           "having options" options)
    (when-not (or (seq path) (:allow-nil-or-empty-path? options))
      (throw (IllegalArgumentException. (str "Empty or nil path in merge-at not allowed. "
                                             "Use :allow-nil-or-empty-path? option to override."))))
    (let [filled-path (pathops/fill-path path path-vars)
          updated-vo (pathops/merge-at (get-vo this vo) filled-path value)]
      (put-vo this updated-vo (:id updated-vo))))

  (remove-from [this vo path path-vars]
    (remove-from this vo path path-vars {}))

  (remove-from [this vo path path-vars options]
    (assert-not-nil-id vo)
    (debug "Removing value from vo identified by" vo "on path" path "with vars" path-vars
           "having options" options)
    (let [filled-path (pathops/fill-path path path-vars)
          updated-vo (pathops/remove-from (get-vo this vo) filled-path)]
      (put-vo this updated-vo (:id updated-vo))))

  VOSearchProxy
  (search [this vo]
    (search this vo {}))

  (search [this vo options]
    (debug "Searching for VOs based on" vo "having options" options "in" @data)
    (let [vos (vals (get @data (votype->hex vo)))]
      (doall (es-filter options (filter (partial matches? vo) vos))))))


;;; A constructor function.

(defn memory-vo-proxy
  "Create a new MemoryVOProxy, initially having no data."
  []
  (MemoryVOProxy. (atom {})))
