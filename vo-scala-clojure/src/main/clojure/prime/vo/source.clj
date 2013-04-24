;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.source
  (:require prime.types)
  (:use clojure.lang))

(def-existing-protocol ValueSource
  (^int     typeID   [this, ^int baseTypeID])

  (^boolean contains [this, ^String name, ^int idx])
  (^int     intAt    [this, ^String name, ^int idx] [this, ^String name, ^int idx, ^int    notFound])
  (^double  doubleAt [this, ^String name, ^int idx] [this, ^String name, ^int idx, ^double notFound])
  (^Object  anyAt    [this, ^String name, ^int idx] [this, ^String name, ^int idx, ^Object notFound]))

(def-existing-protocol ValueSourceable
  (as-source [this] [this valueobject-definition]))

(defmacro def-valuesource [typeName args & definitions]
  `(deftype ~typeName ~args
  prime.vo.source.ValueSourceable
    ~'(as-source [this]   this)
    ~'(as-source [this _] this)
  prime.vo.source.ValueSource
  ~@(concat
      (let [overrides (set (map first definitions))]
        (remove #(overrides (first %)) '[
          (^int    typeID   [this, ^int ID] ID)

          (^int    intAt    [this, ^String name ^int idx ^int    notFound] (prime.types/to-Integer (.anyAt this name idx notFound)))
          (^double doubleAt [this, ^String name ^int idx ^double notFound] (prime.types/to-Decimal (.anyAt this name idx notFound)))

          (^Object anyAt    [this, ^String name ^int idx]  (.anyAt    this name idx nil))
          (^int    intAt    [this, ^String name ^int idx]  (.intAt    this name idx Integer/MIN_VALUE))
          (^double doubleAt [this, ^String name ^int idx]  (.doubleAt this name idx Double/NaN))
        ]))
      definitions)))

;
; PersistentMap implementation
;

(def-valuesource Map-ValueSource [^clojure.lang.IPersistentMap clj-map]
  (contains [this name idx]
    "check contains? documentation at: \n
     http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/contains?"
    (or (contains? clj-map name) (contains? clj-map (keyword name)) (contains? clj-map idx)))

  (anyAt [this, name idx notFound] (or (clj-map name) (clj-map (keyword name)) (clj-map idx) notFound)))

(extend-type clojure.lang.IPersistentMap
  ValueSourceable
  (as-source
    ([this] (->Map-ValueSource this))
    ([this valueobject-definition] (->Map-ValueSource this))))

(extend-type org.msgpack.UnpackResult
  ValueSourceable
  (as-source
    ([this] (.getData this))
    ([this valueobject-definition] (.getData this))))

(extend-type prime.vo.mutable.ValueObject
  ValueSourceable
  (as-source
    ([this] (new prime.vo.source.MutableVOValueSource this))
    ([this valueobject-definition] (as-source this))))

;
; String ValueSources
;

(defonce ^:dynamic *string-source-map* {})

(extend-type java.lang.String
  ValueSourceable
  (as-source
    ([string target-VO-ID]
      (if-let [func (*string-source-map* target-VO-ID)] (as-source (func string))))))

(defn def-string-source [target-VO-type, func]
  (alter-var-root #'*string-source-map* assoc target-VO-type func))
