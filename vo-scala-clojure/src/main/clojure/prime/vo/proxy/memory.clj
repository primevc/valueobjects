;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.proxy.memory
  "An implementation of the VOProxy protocol, keeping all data in
  memory. This implementation is mostly suitable for unit testing or
  interactive development."
  (:require [prime.vo :refer (manifest)]
            [prime.vo.proxy :refer :all]
            [prime.vo.pathops :as pathops]
            [taoensso.timbre :refer (trace debug info warn error fatal spy)]))


;;; Helper functions.

(defn- votype->hex
  "Evaluates to a HEX String representation of the type of the supplied
  VO."
  [vo]
  (Integer/toHexString (.ID (manifest vo))))


;;; Define the in-memory proxy, both implementing VOProxy as well as
;;; VOSearchProxy. The MemoryVOProxy takes an atom encapsulating a map as its
;;; sole argument.

(deftype MemoryVOProxy [data]
  VOProxy
  (get-vo [this vo]
    (debug "Getting VO based on" vo)
    (get-in @data [(votype->hex vo) (:id vo)]))

  (put-vo [this vo]
    (put-vo this vo {}))

  (put-vo [this vo options]
    (debug "Putting VO" vo)
    (swap! data assoc-in [(votype->hex vo) (:id vo)] vo))

  (update [this vo]
    (update this vo (:id vo) {}))

  (update [this vo id]
    (assert (not (nil? id)) (print-str "id required, but nil id supplied for vo:" vo))
    (update this vo id {}))

  (update [this vo id options]
    (debug "Updating VO" vo "having ID" id "with options" options)
    (swap! data assoc-in [(votype->hex vo) id] (assoc vo :id id)))

  (delete [this vo]
    (delete this vo {}))

  (delete [this vo options]
    (debug "Deleting VO based on" vo "with options" options)
    (swap! data update-in [(votype->hex vo)] dissoc (:id vo)))

  (append-to [this vo path path-vars value]
    (append-to this vo path path-vars value {}))

  (append-to [this vo path path-vars value options]
    (debug "Appending" value "identified by" vo " on path" path "with vars" path-vars
           "having options" options)
    (let [filled-path (pathops/fill-path path path-vars)
          updated-vo (pathops/append-to-vo (get-vo this vo) filled-path value)]
      (update this updated-vo (:id updated-vo))))

  (insert-at [this vo path path-vars value]
    (insert-at this vo path path-vars value {}))

  (insert-at [this vo path path-vars value options]
    (debug "Inserting" value "identified by" vo " on path" path "with vars" path-vars
           "having options" options)
    (let [filled-path (pathops/fill-path path path-vars)
          updated-vo (pathops/insert-at (get-vo this vo) (vec filled-path) value)]
      (update this updated-vo (:id updated-vo))))

  (move-to [this vo path path-vars to]
    (move-to this vo path path-vars to {}))

  (move-to [this vo path path-vars to options]
    (debug "Moving value identified by" vo "on path" path "with vars" path-vars
           "having options" options "to" to)
    (let [filled-path (pathops/fill-path path path-vars)
          updated-vo (pathops/move-vo-to (get-vo this vo) (vec filled-path) to)]
      (update this updated-vo (:id updated-vo))))

  (replace-at [this vo path path-vars value]
    (replace-at this vo path path-vars value {}))

  (replace-at [this vo path path-vars value options]
    (debug "Replacing value identified by" vo "on path" path "with vars" path-vars
           "having options" options "with" value)
    (let [filled-path (pathops/fill-path path path-vars)
          updated-vo (pathops/replace-at (get-vo this vo) (vec filled-path) value)]
      (update this updated-vo (:id updated-vo))))

  (merge-at [this vo path path-vars value]
    (merge-at this vo path path-vars value {}))

  (merge-at [this vo path path-vars value options]
    (debug "Merging" value "identified by" vo "on path" path "with vars" path-vars
           "having options" options)
    (let [filled-path (pathops/fill-path path path-vars)
          updated-vo (pathops/merge-at (get-vo this vo) filled-path value)]
      (update this updated-vo (:id updated-vo))))

  (remove-from [this vo path path-vars]
    (remove-from this vo path path-vars {}))

  (remove-from [this vo path path-vars options]
    (debug "Removing value from vo identified by" vo "on path" path "with vars" path-vars
           "having options" options)
    (let [filled-path (pathops/fill-path path path-vars)
          updated-vo (pathops/remove-from (get-vo this vo) (vec filled-path))]
      (update this updated-vo (:id updated-vo))))

  VOSearchProxy
  (search [this vo]
    (search this vo {}))

  (search [this vo options]
    (debug "Searching for VOs based on" vo "having options" options)
    (vals (get @data (votype->hex vo)))))


;;; A constructor function.

(defn memory-vo-proxy
  "Create a new MemoryVOProxy, initially having no data."
  []
  (MemoryVOProxy. (atom {})))
