;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.utils.msgpack
  (:import [org.msgpack.object ArrayType BigIntegerTypeIMPL BooleanType
                               DoubleTypeIMPL FloatTypeIMPL LongIntegerTypeIMPL MapType NilType
                               RawType ShortIntegerTypeIMPL]
           [prime.utils.msgpack MessagePackValueSource MessagePackObjectId]))

(defn as-clojure
  "Get the Clojuresque type for the given MessagePackObject."
  [obj]
  (condp instance? obj
    ArrayType              (reduce (fn [v o] (conj v (as-clojure o))) [] (.asArray ^ArrayType obj))
    BigIntegerTypeIMPL     (.asBigInteger ^BigIntegerTypeIMPL obj)
    BooleanType            (.asBoolean ^BooleanType obj)
    DoubleTypeIMPL         (.asDouble ^DoubleTypeIMPL obj)
    FloatTypeIMPL          (.asFloat ^FloatTypeIMPL obj)
    LongIntegerTypeIMPL    (.asLong ^LongIntegerTypeIMPL obj)
    MapType                (reduce (fn [m [k v]] (assoc m (as-clojure k) (as-clojure ^MapType v)))
                                   {} (.asMap obj))
    NilType                nil
    RawType                (.asString ^RawType obj) ;;---TODO: Is this always correct? Add option?
    ShortIntegerTypeIMPL   (.asInt ^ShortIntegerTypeIMPL obj)
    MessagePackValueSource obj
    MessagePackObjectId    (.oid ^MessagePackObjectId obj)))

(defn ValueSource->map [^MessagePackValueSource vsrc]
  (->> (map (fn [id value]
              [ (Integer/toHexString id),
                (if (instance? MessagePackValueSource value) (MessagePackValueSource->map value)
                #_else (as-clojure value)) ])
            (. vsrc ids)
            (. vsrc values))
    (into {})))
