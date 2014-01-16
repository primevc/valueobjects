;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.utils.msgpack)

(defn as-clojure
  "Get the Clojuresque type for the given MessagePackObject."
  [obj]
  (condp instance? obj
    org.msgpack.object.ArrayType (reduce (fn [v o] (conj v (as-clojure o))) [] (.asArray obj))
    org.msgpack.object.BigIntegerTypeIMPL (.asBigInteger obj)
    org.msgpack.object.BooleanType (.asBoolean obj)
    org.msgpack.object.DoubleTypeIMPL (.asDouble obj)
    org.msgpack.object.FloatTypeIMPL (.asFloat obj)
    org.msgpack.object.LongIntegerTypeIMPL (.asLong obj)
    org.msgpack.object.MapType (reduce (fn [m [k v]] (assoc m (as-clojure k) (as-clojure v)))
                                       {} (.asMap obj))
    org.msgpack.object.NilType nil
    org.msgpack.object.RawType (.asString obj) ;;---TODO: Is this always correct? Add option?
    org.msgpack.object.ShortIntegerTypeIMPL (.asInt obj)
    prime.utils.msgpack.MessagePackValueSource obj
    prime.utils.msgpack.MessagePackObjectId (.oid obj)))
