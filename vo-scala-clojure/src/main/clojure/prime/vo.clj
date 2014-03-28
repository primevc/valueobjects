;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo
  (:refer-clojure :exclude [keyword])
  (:require [clojure.set :as s]
            [fast-zip.core :as zip])
  (:import  [prime.vo IDField ValueObject ValueObjectField VOValueObjectField ValueObjectManifest ValueObjectManifest_0 ValueObjectManifest_1 ValueObjectManifest_N ID]
            [prime.types package$ValueType package$ValueTypes$Tarray package$ValueTypes$Tdef VORef]))

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

(defn ^ValueObjectManifest manifest [in]
  (condp instance? in
    ValueObjectManifest  in
    ValueObject          (.voManifest ^ValueObject in)
  ))

(defn keyword "Returns the :keyword of a ValueObjectField" [^ValueObjectField field]
  (.keyword field))

(defn fields [in]
  (let [in (manifest in)] (condp instance? in
    ValueObjectManifest_N (remove nil? (.. ^ValueObjectManifest_N in fields))
    ValueObjectManifest_1 (remove nil? (cons (.. ^ValueObjectManifest_1 in first) nil))
    ValueObjectManifest_0 nil)
  ))

(defn subtypes [vo-or-manifest]
  (.subtypes (manifest vo-or-manifest)))

(defn fields-with-subtypes [in]
  (distinct (mapcat fields (cons in (subtypes in)))))

(defn field-set
  "Returns a set of field keywords known for the given `VO or Manifest`"
  [vo-or-manifest]
  (set (map keyword (fields vo-or-manifest))))

(defn fields->keyword-set
  "Transforms a seq of ValueObjectFields into a set of field keywords."
  [field-seq]
  (->> field-seq (map keyword) (set)))

(defn field-filtered-seq
  "Returns a sequence sequence of ValueObjectFields, filtered by the (optional)
  `only + exclude field-keyword set`, for the given `VO or Manifest` and all of it's subtypes."
  [vo-or-manifest, only, exclude]
  (let [only       (set only   )
        exclude    (set exclude)
        all-fields (fields-with-subtypes vo-or-manifest)
        all-keys   (fields->keyword-set all-fields)
        field-keys (if-not (empty? only)    (s/select     only       all-keys) all-keys)
        field-keys (if-not (empty? exclude) (s/difference field-keys exclude)  field-keys)]
    (assert (empty? (s/difference only    all-keys)) (str vo-or-manifest ":only    " only    " contains key not present in VO (or subtype) field-set: " all-keys))
    (assert (empty? (s/difference exclude all-keys)) (str vo-or-manifest ":exclude " exclude " contains key not present in VO (or subtype) field-set: " all-keys))
    (filter (comp field-keys keyword) all-fields)))

(defn ^ValueObjectField find-own-field
  "Searches for a field by key. Returns nil if not found within the given ValueObject(manifest) type."
  [vo-or-manifest key]
  (.findOrNull (manifest vo-or-manifest) key))

(defn ^ValueObjectField field-or-subtype-field
  "Searches for a field by key in the given ValueObject(manifest) type and all known subtypes.

  - Throws if no ValueObject which extends the given type, has a field known by 'key.
  - Throws when the given key is ambiguous."
  [vo-or-manifest key]
  {:pre [key]}
  (or (find-own-field vo-or-manifest key)
      (let [manifest (manifest vo-or-manifest)
            fields   (remove nil? (map #(.findOrNull ^ValueObjectManifest % key) (.subtypes manifest)))
            fcount   (count fields)]
          (assert (<= fcount 1) (print-str "Ambiguous: `" key "` maps to multiple fields: " fields))
          (assert (=  fcount 1) (print-str "No field found `" key "` in " manifest " or subtypes."))
              (first fields))))

(defn vo-field? [field]
  (or (instance? VOValueObjectField field) (instance? package$ValueTypes$Tdef field)))

(defn array-field? [field]
  (or (instance? package$ValueTypes$Tarray field)
      (and (instance? ValueObjectField field) (array-field? (.valueType ^ValueObjectField field)))))

(defn ^ValueObject type-default-vo [^package$ValueType field-type]
  (condp instance? field-type
    ValueObjectField          (type-default-vo (.valueType ^ValueObjectField field-type))
    package$ValueTypes$Tdef   (.empty ^package$ValueTypes$Tdef field-type)
    package$ValueTypes$Tarray (type-default-vo (.innerType ^package$ValueTypes$Tarray field-type))
    nil))

(defn fields-path-seq
  "Recursively walks a 'path seq and translates: keywords, numbers and strings to: ValueObjectFields.
   Applies the optional field-transform to each found field.

   Returns anything not found as ValueObjectField as is.

   Valid paths to ValueObject fields within arrays are:
    [:booklet :spreads {:id \"asdf\"} :tags]
    [:booklet :spreads 0 :tags]
    [:booklet :spreads \"*\" :tags]
    [:booklet :spreads * :tags]"

  ([vo path] (fields-path-seq vo path identity))

  ([^ValueObject vo [first-step next-step & path :as full-path] field-transform]
    (assert (or (empty? full-path) (instance? ValueObject vo))
              (print-str vo "is not a ValueObject (possibly a VO leaf node). full-path = " full-path))
    (if-let [field (if first-step (field-or-subtype-field vo first-step))]
      (cons (field-transform field)
        (if next-step
          (if (array-field? field)
            (do (assert (or (integer? next-step) (map? next-step) (= * next-step) (= "*" next-step))
                        (print-str "\n  A path-node poiting inside a vector should be: * (for any), an integer index  or an {:id map} -- Instead " next-step " was given."))
                (if-let [inner-vo (type-default-vo field)]
                  ; This is an array of VOs.
                  (cons (cond
                          (= "*" next-step)
                            *
                          (map?  next-step)
                            (let [[k v] (first next-step)]
                              (assert (= 1 (count next-step)) (print-str "Search in array path " field " only supported for 1 key. Instead: " next-step))
                              { (field-transform (field-or-subtype-field inner-vo k)) v })
                          :else
                            next-step)
                        (if path (fields-path-seq inner-vo path field-transform) #_else nil))
                ; else, if not a ValueObject array field, path should end now.
                  (do (assert (empty? path) (print-str "Trailing path elements: " (next full-path) " cannot be followed. " first-step " is not an array of ValueObjects."))
                      (next full-path))))
          ; else, if not an array field:
            (fields-path-seq (.defaultValue field) (next full-path) field-transform)
       )))
    #_else
        (next full-path)))) ; No field found, return the rest of the path as is.

(defn ^ValueObjectField id-field [vo-or-manifest]
  (let [m (manifest vo-or-manifest)]
    (if (instance? IDField m) (._id ^IDField m))))

(defn has-id? [^ValueObject vo]
  "Check if the ValueObject has an ID-field and this field has a value."
  (let [field (id-field vo)]
    (and field (. field in vo))))

(definline ^{:doc
  "Gets the :id of a map, ValueObject or VORef.
  If input is neither, just returns the input."}
  id [input]
  `(let [v# ~input] (if (instance? VORef v#) (._id ^VORef v#) (or (:id v#) v#))))

(defn id=
  "Compare the id of `a` and `b`.
    Useful to check if:
    - a VORef refers to a particular object
    - two ValueObjects share the same :id"
  ([x] true)
  ([x y] (= (id x) (id y)))
  ([x y & more]
   (if (= (id x) (id y))
    (if (next more)
      (recur y (first more) (next more))
      (id=   y (first more)))
    false)))

(defn vo+subtypes-manifests-seq "Builds a seq of all subtype manifests." [^ValueObjectManifest voManifest]
  (cons voManifest (mapcat vo+subtypes-manifests-seq (.subtypes voManifest))))


;;; A zipper definition for value objects.

(defn vo-zipper [vo]
  "Creates a zipper for a value object. Children of a value object
  node are represented by a sequence of key-value pairs."
  (zip/zipper (fn [node]           ; branch?
                (or (instance? ValueObject node)
                    (vector? node)))
              seq                  ; children
              (fn [node children]  ; make-node
                (if (instance? ValueObject node)
                  (into (.. ^ValueObject node voCompanion empty) children)
                  (vec children)))
              vo))
