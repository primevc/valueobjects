;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.util.cassandra-history
  (:refer-clojure :exclude [get])
  (require 
    [qbits.alia :as alia]
    [qbits.tardis :as tardis])
  (:import 
    [prime.utils.msgpack.VOPacker]
    [prime.utils.msgpack.VOUnpacker]
    [java.io.ByteArrayOutputStream]
    [java.io.ByteArrayInputStream]
    [prime.vo ValueObject]
    [org.msgpack.Unpacker]
    )
)

(defn ObjectId->byte-buffer [^org.bson.types.ObjectId oid] (java.nio.ByteBuffer/wrap (.toByteArray oid)))

(defn simple-prime-type->cql-type [type-keyword]
  (case type-keyword
    :prime.types/ObjectId   [ :blob       ObjectId->byte-buffer   ]
    :prime.types/integer    [ :int        prime.types/to-Integer  ]
    :prime.types/Color      [ :int        prime.types/to-Integer  ]
    :prime.types/String     [ :varchar    prime.types/to-String   ]
    :prime.types/URI        [ :varchar    prime.types/to-String   ]
    :prime.types/URL        [ :varchar    prime.types/to-String   ]
    :prime.types/E-mail     [ :varchar    prime.types/to-String   ]
    :prime.types/FileRef    [ :varchar    prime.types/to-String   ]
    :prime.types/boolean    [ :boolean    prime.types/to-Boolean  ]
    :prime.types/decimal    [ :double     prime.types/to-Decimal  ]
    :prime.types/Date       [ :timestamp  prime.types/to-DateTime ]
    :prime.types/Date+time  [ :timestamp  prime.types/to-DateTime ]
))

(defn ->byte-array [value]
  (let [
    out (java.io.ByteArrayOutputStream.)
    msgpack (prime.utils.msgpack.VOPacker. out)
    pack (.pack msgpack value)]
    (.toByteArray out)))

(defn VOChange->byte-array [value path]
  (let [
    out (java.io.ByteArrayOutputStream.)
    msgpack 
      (doto (prime.utils.msgpack.VOPacker. out)
        (.packArray (if path 2 #_else 1))
        (.pack value))
    ]
    (when path (.pack msgpack path))
    (.close out)
    (.toByteArray out)))

(defn byte-array->VOChange [bytes vo]
    (let [unpacker 
      (doto (org.msgpack.Unpacker. (org.apache.cassandra.utils.ByteBufferUtil/inputStream bytes))
              (.setVOHelper prime.utils.msgpack.VOUnpacker$/MODULE$))
      array (.. unpacker next getData asArray)]
      [ (.. vo voCompanion (valueOf (nth array 0))) ; vo
        (.asString (nth array 1)) ; path
      ]
      ))

(def actions {
  :put (int 1)
  :update (int 2)
  :delete (int 3)
  :vmove (int 4)
  :vput (int 5)
  :vdelete (int 6)
  })

(defn get-table-name [vo]
  (str "t" (Integer/toHexString (.. vo voManifest ID))))

(defn get [cluster vo] 
  ; This should just send all records.
  (let [result (alia/with-session cluster 
                (alia/execute 
                  (alia/prepare (apply str "SELECT * FROM " (get-table-name vo) " WHERE id = ?")) :values [(:id vo)]))]
    ;TODO: Create a custom ValueSource for history data, keeping the UUID and action around.
    (first (byte-array->VOChange ((last result) "data") vo))))

(defn get-slice [cluster vo]
  ; This should send a merge of all records since the last put.  
  )

(defn put [cluster vo options]
  (assert (:id vo) "vo requires an id")
  (prn (type vo))
  (let [action (or (-> options :action) :put)
    idconv (second (simple-prime-type->cql-type (.. vo voManifest _id valueType keyword)))
    ]
    (alia/with-session cluster 
      (alia/execute (alia/prepare 
        (apply str "INSERT INTO " (get-table-name vo) " (version, id, action, data) VALUES ( ? , ? , ? , ? )")) 
        :values [ (tardis/unique-time-uuid (.getTime (java.util.Date.))) 
                  (idconv (:id vo)) ; Convert id to proper type.
                  (-> actions action) 
                  (java.nio.ByteBuffer/wrap (VOChange->byte-array  vo ""))]))))

(defn appendTo [cluster vo id options]
  (put cluster (conj vo {:id id}) (conj options {:action :vput})))

(defn update [cluster vo id options]
  (put cluster (conj vo {:id id}) (conj options {:action :update})))

(defn delete [cluster vo options]
  (put cluster vo (conj options {:action :delete})))
