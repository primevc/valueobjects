;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.util.cassandra-history
  (:refer-clojure :exclude [get])
  (require 
    [qbits.alia               :as alia]
    [qbits.tardis             :as tardis]
    [immutant.cache           :as cache]
    )
  (:import 
    [prime.utils.msgpack.VOPacker]
    [prime.utils.msgpack.VOUnpacker]
    [java.io.ByteArrayOutputStream]
    [java.io.ByteArrayInputStream]
    [prime.vo ValueObject]
    [org.msgpack.Unpacker]
    )
)

(defonce latest-history-put (cache/cache "latest-history-put" :locking :pessimistic))

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
        (.pack value))
    ]
    (when path (.pack msgpack path))
    (.close out)
    (.toByteArray out)))

(defn byte-array->VOChange [bytes vo]
  ;(apply prn (map #(Integer/toHexString (.get bytes %)) (range (.position bytes) (.capacity bytes))))
  (let [ unpacker
    (doto (org.msgpack.Unpacker. (org.apache.cassandra.utils.ByteBufferUtil/inputStream (.slice bytes)))
      (.setVOHelper prime.utils.msgpack.VOUnpacker$/MODULE$))
    vosource (.. unpacker next getData)
    path     (.. unpacker next getData)]
    [ (.. vo voCompanion (valueOf vosource))
      (if path (.asInt path)) ]))

(defn idconv [vo]
  ((second (simple-prime-type->cql-type (.. vo voManifest _id valueType keyword))) (:id vo)))

(defn keyword-keys [m]
  (into (empty m) (for [[k v] m] [(keyword (.toLowerCase k)) v])))

(def actions {
  :put (int 1)
  :update (int 2)
  :delete (int 3)
  :appendTo (int 5)
  :moveTo (int 4)
  :remove (int 6)
  :insertAt (int 7)
  :replace (int 8)
  })

(defn get-table-name [vo]
  (str "t" (Integer/toHexString (.. vo voManifest ID))))

(defn get-latest-put [vo cluster]
  (let [result (alia/with-session cluster
        (alia/execute
          (alia/prepare (apply str "SELECT version,action FROM " (get-table-name vo) " WHERE id = ? ORDER BY version DESC;")) :values [(idconv vo)]))]
      ; Find first version where action == (:put actions)
      (first (filter #(= (% "action") (:put actions)) result))))

(defn build-vo [res vo]
  (loop [i 0 c nil] ; i = counter, c = current result.
    (let [row (nth res i {})]
      (let [c
        (cond 
          (= (row "action") (:put actions))
            (first (byte-array->VOChange (row "data") vo))
          (= (row "action") (:update actions))
            (conj c (first (byte-array->VOChange (row "data") vo)))
          (= (row "action") (:delete actions))
            (empty c)
          ;(= (:action row) (:vmove actions))
          ;  (do)
          ;(= (:action row) (:vput actions))
          ;  (do)
          ;(= (:action row) (:vdelete actions))
          ;  (do)
          :else
            c
          )
        ]
      (if-not (empty? row) 
        (recur (inc i) c) 
        #_else 
        (if (empty? c) nil c))))))

(defn get [cluster vo] 
  ; This should send a merge of all records since the last put.  
  (let [
    last-put (or (:version ((keyword (str (:id vo))) latest-history-put)) ((get-latest-put vo cluster) "version"))
    result 
      (alia/with-session cluster
        (alia/execute 
          (alia/prepare (apply str "SELECT * FROM " (get-table-name vo) " WHERE id = ? and version >= ?;")) :values [(idconv vo) last-put]))]
      (build-vo result vo)))

(defn get-slice [cluster vo options]
  ; This should just send all records.
  (let [
        result (alia/with-session cluster 
                (alia/execute 
                  (alia/prepare (apply str "SELECT * FROM " (get-table-name vo) " WHERE id = ?")) :values [(idconv vo)]))
        slices (or (:slices options) 1)]
    ;TODO: Create a custom ValueSource for history data, keeping the UUID and action around.
    (take slices (byte-array->VOChange ((last result) "data") vo))))

(defn put [cluster vo options]
  ; TODO: If action is put, save version into immutant cache.
  (assert (:id vo) "vo requires an id")
  (let [action (or (-> options :action) :put)]
    (alia/with-session cluster 
      (alia/execute (alia/prepare 
        (apply str "INSERT INTO " (get-table-name vo) " (version, id, action, data) VALUES ( ? , ? , ? , ? )")) 
        :values [ (tardis/unique-time-uuid (.getTime (java.util.Date.))) 
                  (idconv vo) ; Convert id to proper type.
                  (-> actions action) 
                  (java.nio.ByteBuffer/wrap (VOChange->byte-array  vo (:path options)))]))))

(defn update [cluster vo id options]
  (put cluster (conj vo {:id id}) (conj options {:action :update})))

(defn delete [cluster vo options]
  (put cluster vo (conj options {:action :delete})))

(defn appendTo [cluster vo id options]
  (put cluster (conj vo {:id id}) (conj options {:action :appendTo})))

(defn insertAt [this vo id path options]
  (put cluster (conj vo {:id id}) (conj options {:action :insertAt})))

(defn moveTo [this vo id path options]
  (put cluster (conj vo {:id id}) (conj options {:action :moveTo})))

(defn replaceAt [this vo id path options]
  (put cluster (conj vo {:id id}) (conj options {:action :replaceAt})))

(defn remove [this vo id path options]
  (put cluster (conj vo {:id id}) (conj options {:action :remove})))
