;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.util.cassandra-history
  (:refer-clojure :exclude [get])
  (require prime.vo
    [qbits.alia                   :as alia]
    [qbits.tardis                 :as tardis]
    [prime.vo.util.elasticsearch  :as es]
    [prime.vo.pathops             :as pathops]
    )
  (:import 
    [prime.utils.msgpack VOPacker VOUnpacker]
    [java.io ByteArrayOutputStream ByteArrayInputStream]
    [prime.vo ValueObject ValueObjectField]
    [org.msgpack Unpacker MessagePackObject]
    )
)

(defn get-table-name [vo]
  (str "t" (Integer/toHexString (.ID (prime.vo/manifest vo)))))

;
; PUT version caching
;

(defn ^:dynamic mk-last-put-cache "
  Create a cache per VO type: called and memoized by last-put-cache.

  This fn creates a cache (map)
    wherein the last version id of a PUT operation per VO-id is stored
    to speed up retrieval of VO changes since the last put.

  This should improve speed of VOProxy/get: replaying only the VO changes since the last PUT.

  If running on immutant, the default implementation returns an immutant.cache named: `${VO table name}-last-put`."
  [vo]
  (try
    (require 'immutant.cache)
    (eval (list 'immutant.cache/cache (str (get-table-name vo) "-last-put") :locking :pessimistic))
  (catch Exception e)))

(def ^:private mk-last-put-cache-memoized (memoize #'mk-last-put-cache))

(defn last-put-cache [vo]
  (mk-last-put-cache-memoized (prime.vo/manifest vo)))

;
; Column serialization
;

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

(defn replace-star [path]
  (map #(do
      (cond
          (= % *)
            nil
          (and (map? %) (= (val (first %)) *))
            {(key (first %)) nil}
          :else
            %
        )) path))

(defn VOChange->byte-array
  ([^ValueObject vo] (VOChange->byte-array vo nil))
    
  ([^ValueObject vo path & options]
      (let [out (java.io.ByteArrayOutputStream.)
            msgpack (doto (prime.utils.msgpack.VOPacker. out)
                      (.pack vo))]
        (when path
          (.pack msgpack (replace-star (prime.vo/fields-path-seq vo path #(.id ^ValueObjectField %)))))
        (doseq [option options]
          (let [option (if (and (empty? option) (map? option)) nil option)
                option (if (and (coll? option) (not (instance? ValueObject option))) (apply list option) option)]
            (.pack msgpack option)
            ))
        (.close out)
        (.toByteArray out))))

(defn byte-array->VOChange [bytes vo]
  ; (apply prn (map #(Integer/toHexString (.get bytes %)) (range (.position bytes) (.capacity bytes))))
  (let [ 
    bytes (.slice bytes)
    ;pp (apply prn (map #(Integer/toHexString (.get bytes %)) (range (.position bytes) (.capacity bytes))))
    unpacker  (doto (org.msgpack.Unpacker. (org.apache.cassandra.utils.ByteBufferUtil/inputStream bytes))
                (.setVOHelper prime.utils.msgpack.VOUnpacker$/MODULE$))
    [vosource integer-path & options] (iterator-seq (.. unpacker iterator))
    path (map (fn [^MessagePackObject item] (if (.isNil item) nil #_else (.asInt item))) (if integer-path (.asArray integer-path)))
    ]
    
    (cons (.. vo voCompanion (apply vosource)) (cons path options))
  ))

(defn idconv [vo]
  ((second (simple-prime-type->cql-type (.. vo voManifest _id valueType keyword))) (:id vo)))

(defn keyword-keys [m]
  (into (empty m) (for [[k v] m] [(keyword (.toLowerCase k)) v])))

; TODO: VOAction enum definition
(def actions {
  :put (int 1)
  :update (int 2)
  :delete (int 3)
  :append-to (int 5)
  :move-to (int 4)
  :remove-from (int 6)
  :insert-at (int 7)
  :replace-at (int 8)
  :merge-at (int 9)
  })

(defn get-latest-put [vo cluster]
  (let [result (alia/with-session cluster
        (alia/execute
          (alia/prepare (apply str "SELECT version,action FROM " (get-table-name vo) " WHERE id = ? ORDER BY version DESC;")) :values [(idconv vo)]))]
      ; Find first version where action == (:put actions)
      (first (filter #(= (:action %) (:put actions)) result))))

(defn build-vo [res vo]
  (loop [i 0 c nil] ; i = counter, c = current result.
    (let [row (nth res i {})]
      (let [c
        (cond
          (= (:action row) (:put actions))
            (first (byte-array->VOChange (:data row) vo))
          
          (= (:action row) (:update actions))
            (conj c (first (byte-array->VOChange (:data row) vo)))
          
          (= (:action row) (:delete actions))
            (empty c)
          
          (= (:action row) (:append-to actions))
            (let [  [vo path path-vars value options] (byte-array->VOChange (:data row) vo)
                    path (pathops/fill-path path path-vars)]
              (pathops/append-to-vo vo path value))
          
          (= (:action row) (:move-to actions))
            (let [  [vo path path-vars to options] (byte-array->VOChange (:data row) vo)
                    path (pathops/fill-path path path-vars)]
              (pathops/move-vo-to vo path to))
          
          (= (:action row) (:remove-from actions))
            (let [  [vo path path-vars options] (byte-array->VOChange (:data row) vo)
                    path (pathops/fill-path path path-vars)]
              (pathops/remove-from vo path))
          
          (= (:action row) (:insert-at actions))
            (let [  [vo path path-vars value options] (byte-array->VOChange (:data row) vo)
                    path (pathops/fill-path path path-vars)]
              (pathops/insert-at vo path value))
          
          (= (:action row) (:replace-at actions))
            (let [  [vo path path-vars value options] (byte-array->VOChange (:data row) vo)
                    path (pathops/fill-path path path-vars)]
              (pathops/replace-at vo path value))
          
          (= (:action row) (:merge-at actions))
            (let [  [vo path path-vars value options] (byte-array->VOChange (:data row) vo)
                    path (pathops/fill-path path path-vars)]
              (pathops/merge-at vo path value))
          
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
    last-put (:version (or (clojure.core/get (last-put-cache vo) (str (:id vo))) (get-latest-put vo cluster)))
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
    (take slices (byte-array->VOChange (:data (last result)) vo))))

(defn- insert [cluster vo id action data]
  (alia/with-session cluster
      (alia/execute (alia/prepare
        (apply str "INSERT INTO " (get-table-name vo) " (version, id, action, data) VALUES ( ? , ? , ? , ? )"))
        :values [(tardis/unique-time-uuid (.getTime (java.util.Date.))) id (actions action) (java.nio.ByteBuffer/wrap data)])))

(defn put [cluster vo options]
  ; TODO: If action is put, save version into immutant cache.
  (assert (:id vo) "vo requires an id")
  (insert cluster
          vo
          (idconv vo)
          :put
          (VOChange->byte-array vo)))

(defn update [cluster vo id options]
  (insert cluster
          vo
          (idconv (conj vo {:id id}))
          :update
          (VOChange->byte-array vo)))

(defn delete [cluster vo options]
  (insert cluster
          vo
          (idconv vo)
          :delete
          nil))

(defn append-to [cluster vo path path-vars value options]
  (insert cluster
          vo
          (idconv vo)
          :append-to
          (VOChange->byte-array vo path path-vars value options)))

(defn insert-at [cluster vo path path-vars value options]
  (insert cluster
          vo
          (idconv vo)
          :insert-at
          (VOChange->byte-array vo path path-vars value options)))

(defn move-to [cluster vo path path-vars to options]
  (insert cluster
          vo
          (idconv vo)
          :move-to
          (VOChange->byte-array vo path path-vars to options)))

(defn replace-at [cluster vo path path-vars value options]
  (insert cluster
          vo
          (idconv vo)
          :replace-at
          (VOChange->byte-array vo path path-vars value options)))

(defn merge-at [cluster vo path path-vars value options]
  (insert cluster
          vo
          (idconv vo)
          :merge-at
          (VOChange->byte-array vo path path-vars value options)))

(defn remove-from [cluster vo path path-vars options]
  (insert cluster
          vo
          (idconv vo)
          :remove-from
          (VOChange->byte-array vo path path-vars options)))
