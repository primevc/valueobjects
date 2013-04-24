;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo
  (:require [clojure.set     :as    s])
  (:import  [prime.vo IDField ValueObject ValueObjectField ValueObjectManifest ValueObjectManifest_0 ValueObjectManifest_1 ValueObjectManifest_N ID]
            [prime.types package$ValueType package$ValueTypes$Tarray package$ValueTypes$Tdef]))

(set! *warn-on-reflection* true)

(defonce ^{:dynamic true :doc "
  Used to fetch a ValueObject when dereferencing VORef properties.
  A function from VOType => deref-function.

  Where deref-function is:
    - A function from ID => ValueObject: (fn [id] vo...)
    - Looks up an ID and returns the corresponding ValueObject.
      (Can be a map, VOProxy or custom function.)

  Initially {}

  Usage:
    (binding [*deref-map* { MyReferencedValueObject (my-valueobject-proxy ...) }] @(:voref-field vo))
  "
} *deref-map* {})

(def ^:dynamic *voseq-key-fn* "
  A function from ValueObjectField => Any, or nil.
  When not nil, creating a seq from ValueObject will call this function to produce the key value.
  Initially nil.

  Usage example:
    (binding [*voseq-key-fn* #(.id ^ValueObjectField %)] (map name vo))
  " nil)

(defn fields [in]
  (condp instance? in
    ValueObject           (fields (.. ^ValueObject in voManifest))
    ValueObjectManifest_N (remove nil? (.. ^ValueObjectManifest_N in fields))
    ValueObjectManifest_1 (remove nil? (cons (.. ^ValueObjectManifest_1 in first) nil))
    ValueObjectManifest_0 nil))

(defn field-set [vo-or-manifest]
  (set (map #(.keyword ^ValueObjectField %) (fields vo-or-manifest))))

(defn field-filtered-seq [vo-or-manifest, only, exclude]
  (let [only       (set only   )
        exclude    (set exclude)
        all-keys   (field-set vo-or-manifest)
        field-keys (if-not (empty? only)    (s/select     only       all-keys) all-keys)
        field-keys (if-not (empty? exclude) (s/difference field-keys exclude)  field-keys)]
    (assert (empty? (s/difference only    all-keys)) (str ":only    contains key not present in VO field-set: " all-keys))
    (assert (empty? (s/difference exclude all-keys)) (str ":exclude contains key not present in VO field-set: " all-keys))
    (filter
      #(field-keys (.keyword ^ValueObjectField %))
      (fields vo-or-manifest))))

(defn type-default-vo [^package$ValueType field]
  (condp instance? field
    package$ValueTypes$Tdef   (.empty ^package$ValueTypes$Tdef field)
    package$ValueTypes$Tarray (type-default-vo (.innerType ^package$ValueTypes$Tarray field))
    nil))

(defn fields-path-seq [vo [first-field-name & path]]
  (let [^ValueObjectManifest voManifest (if (instance? ValueObject vo) (.voManifest ^ValueObject vo) #_else vo)
        field (. voManifest (findOrNull first-field-name))]
    (if field
      (if path
        (let [inner-vo    (type-default-vo (.valueType field))
              next-fields (fields-path-seq inner-vo path)]
          (if next-fields (cons field next-fields)))
      #_else
        (cons field nil)))))

(defn has-id? [^ValueObject vo]
  (and vo (instance? IDField (. vo voManifest))
    (. (._id ^IDField (. vo voManifest)) in vo)))

(defn vo+subtypes-manifests-seq "Builds a seq of all subtype manifests." [^ValueObjectManifest voManifest]
  (cons voManifest (mapcat vo+subtypes-manifests-seq (.subtypes voManifest))))
