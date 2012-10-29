;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.source
  (:require prime.types))

(defprotocol ValueSource
  (typeID   [this, ^int baseTypeID])

  (contains [this, name, idx])
  (boolAt   [this, name, idx] [this, name, idx, notFound])
  (intAt    [this, name, idx] [this, name, idx, notFound])
  (doubleAt [this, name, idx] [this, name, idx, notFound])
  (anyAt    [this, name, idx] [this, name, idx, notFound]))

(defprotocol ValueSourceable
  (as-source [this] [this valueobject-definition]))

;
; PersistentMap implementation
;

(deftype Map-ValueSource [^clojure.lang.IPersistentMap map]
  prime.vo.source.ValueSource
    (contains [map name idx]
      "check contains? documentation at: \n
       http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/contains?"
      (or (contains? map name) (contains? map idx)))

    (anyAt    [this, name idx notFound] (or (map name) (map (keyword name)) (map idx) notFound))
    (boolAt   [this, name idx notFound] (prime.types/to-Boolean (.anyAt this name idx notFound)))
    (intAt    [this, name idx notFound] (prime.types/to-Integer (.anyAt this name idx notFound)))
    (doubleAt [this, name idx notFound] (prime.types/to-Decimal (.anyAt this name idx notFound)))

    (anyAt    [this, name idx]          (.anyAt    this name idx nil))
    (boolAt   [this, name idx]          (.boolAt   this name idx false))
    (intAt    [this, name idx]          (.intAt    this name idx Integer/MIN_VALUE))
    (doubleAt [this, name idx]          (.doubleAt this name idx Double/NaN))
)

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
