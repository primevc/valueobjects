;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.source
  (:require prime.types)
  (:use clojure.lang))

(def-existing-protocol ValueSource
  (typeID   [this, ^int baseTypeID])

  (contains [this, name, idx])
  (intAt    [this, name, idx] [this, name, idx, notFound])
  (doubleAt [this, name, idx] [this, name, idx, notFound])
  (anyAt    [this, name, idx] [this, name, idx, notFound]))

(def-existing-protocol ValueSourceable
  (as-source [this] [this valueobject-definition]))

(defmacro def-valuesource [typeName args & definitions]
  `(deftype ~typeName ~args
  prime.vo.source.ValueSource
  ~@(concat
      (let [overrides (set (map first definitions))]
        (remove #(overrides (first %)) '[
          (typeID [this ID] ID)

          (intAt    [this, name idx notFound] (prime.types/to-Integer (.anyAt this name idx notFound)))
          (doubleAt [this, name idx notFound] (prime.types/to-Decimal (.anyAt this name idx notFound)))

          (anyAt    [this, name idx]          (.anyAt    this name idx nil))
          (intAt    [this, name idx]          (.intAt    this name idx Integer/MIN_VALUE))
          (doubleAt [this, name idx]          (.doubleAt this name idx Double/NaN))
        ]))
      definitions)))

;
; PersistentMap implementation
;

(def-valuesource Map-ValueSource [^clojure.lang.IPersistentMap clj-map]
  (contains [this name idx]
    "check contains? documentation at: \n
     http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/contains?"
    (or (contains? clj-map name) (contains? clj-map idx)))

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
