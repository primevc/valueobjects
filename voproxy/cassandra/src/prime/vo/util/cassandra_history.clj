;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.util.cassandra-history
  (:refer-clojure :exclude [get])
  (require [containium.systems.cassandra :as cassandra]
           [prime.vo.pathops :as pathops]
           [prime.utils :as utils]
           [clojure.walk :refer (stringify-keys keywordize-keys)]
           [prime.vo]
           [prime.types]
           [prime.vo.util.json])
  (:import [prime.utils.msgpack VOPacker VOUnpacker VOUnpacker$]
           [java.io ByteArrayOutputStream ByteArrayInputStream]
           [java.nio ByteBuffer]
           [prime.vo ValueObject ValueObjectField]
           [org.msgpack Unpacker MessagePackObject]
           [org.bson.types ObjectId]
           [org.apache.cassandra.utils ByteBufferUtil UUIDGen]))


(defn get-table-name [vo]
  "Returns the table name for the given VO."
  (str "t" (Integer/toHexString (.ID (prime.vo/manifest vo)))))


;;; PUT version caching

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


;;; Column serialization

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

;;--- FIXME: quick fix for now, maybe something already exists, otherwise it needs to be moved to
;;           a more general place.
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
    org.msgpack.object.RawType (.asString obj) ;;---TODO: Is this always correct?
    org.msgpack.object.ShortIntegerTypeIMPL (.asInt obj)
    prime.utils.msgpack.MessagePackValueSource obj
    prime.utils.msgpack.MessagePackObjectId (.oid obj)))


(defn as-vo
  [source target-vo]
  (.. target-vo voCompanion (apply source)))


;; (defn ->byte-array [value]
;;   (let [
;;     out (java.io.ByteArrayOutputStream.)
;;     msgpack (prime.utils.msgpack.VOPacker. out)
;;     pack (.pack msgpack value)]
;;     (.toByteArray out)))


(defn replace-star [path]
  (reduce #(cond (= % *) (conj %1 nil)
                 (and (map? %2) (= (val (first %2)) *)) (conj %1 {(key (first %2)) nil})
                 :else (conj %1 %2))
          [] path))


(defn prepare-path
  [vo path]
  (replace-star (prime.vo/fields-path-seq vo path #(.id ^ValueObjectField %))))


(defn change-data->bytes
  [& items]
  (let [out (ByteArrayOutputStream.)
        packer (VOPacker. out)]
    (doseq [item items] (.pack packer item))
    (.toByteArray out)))


(defn bytes->change-data
  [bytes]
  (let [unpacker (doto (Unpacker. (ByteBufferUtil/inputStream bytes))
                   (.setVOHelper VOUnpacker$/MODULE$))]
    (map as-clojure (iterator-seq (.. unpacker iterator)))))


(defn idconv [{:keys [id] :as vo}] {:pre [id]}
  ((second (simple-prime-type->cql-type (.. vo voManifest _id valueType keyword))) id))

(defn keyword-keys [m]
  (into (empty m) (for [[k v] m] [(keyword (.toLowerCase k)) v])))

;;--- TODO: VOAction enum definition
(def actions
  {:put (int 1)
   :update (int 2)
   :delete (int 3)
   :append-to (int 5)
   :move-to (int 4)
   :remove-from (int 6)
   :insert-at (int 7)
   :replace-at (int 8)
   :merge-at (int 9)})


(defn get-latest-put [system consistency vo]
  (let [table-name (get-table-name vo)
        query (str "SELECT version, action FROM " table-name " WHERE id = ? ORDER BY version DESC;")
        prepared (cassandra/prepare system query)
        result (cassandra/do-prepared system prepared
                                      {:consistency consistency :keywordize? true}
                                      [(idconv vo)])]
    ;; Find first version where action == (:put actions)
    (let [put-nr (:put actions)]
      (first (filter #(= (:action %) put-nr) result)))))


(defn build-vo [result target-vo]
  (loop [index 0
         accumulator (assoc (empty target-vo) :id (:id target-vo))]
    (if-let [row (nth result index nil)]
      (recur (inc index)
             (condp = (:action row)
               (:put actions)
               (let [vo-source (first (bytes->change-data (:data row)))]
                 (as-vo vo-source target-vo))

               (:update actions)
               (let [[vo-source _] (bytes->change-data (:data row))
                     update-vo (as-vo vo-source target-vo)]
                 (utils/deep-merge-with (fn [x y] y) accumulator update-vo))

               (:delete actions)
               (empty target-vo)

               (:append-to actions)
               (let [[path path-vars value _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/append-to-vo accumulator filled-path value))

               (:move-to actions)
               (let [[path path-vars to _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/move-vo-to accumulator filled-path to))

               (:remove-from actions)
               (let [[path path-vars _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/remove-from accumulator filled-path))

               (:insert-at actions)
               (let [[path path-vars value _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/insert-at accumulator filled-path value))

               (:replace-at actions)
               (let [[path path-vars value _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/replace-at accumulator filled-path value))

               (:merge-at actions)
               (let [[path path-vars value _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/merge-at accumulator filled-path value))

              :else accumulator))
      (if (empty? accumulator) nil accumulator))))


;;; Proxy functions.

(defn get [{:keys [system consistency]} target-vo]
  ;; This returns a VO having all records sinds the last put re-applied to it.
  (let [last-put (:version (or (clojure.core/get (last-put-cache target-vo) (str (:id target-vo)))
                               (get-latest-put system consistency target-vo)))
        query (str "SELECT * FROM " (get-table-name target-vo) " WHERE id = ? "
                   (when last-put "and version >= ?;"))
        prepared (cassandra/prepare system query)
        args (if last-put [(idconv target-vo) last-put] [(idconv target-vo)])
        result (cassandra/do-prepared system prepared {:consistency consistency :keywordize? true}
                                      args)]
      (build-vo result target-vo)))


;;--- FIXME: Implement this based on dates and slice query.
;;    (and implicit search to latest put for begin date)
(defn get-slice [{:keys [system consistency]} vo {:keys [slices] :or [slices 1]}]
  ;; This should just send all records.
  #_(let [query (str "SELECT * FROM " (get-table-name vo) " WHERE id = ?")
        prepared (cassandra/prepare system query)
        result (cassandra/do-prepared system prepared {:consistency consistency :keywordize? true}
                                      [(idconv vo)])]
    ;;---TODO: Create a custom ValueSource for history data, keeping the UUID and action around.
    (take slices (byte-array->VOChange (:data (last result)) vo))))


(defn- insert [{:keys [system consistency]} vo id action data]
  (let [query (str "INSERT INTO " (get-table-name vo) " (version, id, action, data) "
                   "VALUES ( ? , ? , ? , ? )")
        prepared (cassandra/prepare system query)]
    (cassandra/do-prepared system prepared {:consistency consistency}
                           [(UUIDGen/getTimeUUID) id (actions action)
                            (when data (ByteBuffer/wrap data))])))


(defn put [proxy vo options]
  ;; ---TODO: If action is put, save version into immutant cache.
  (assert (:id vo) "vo requires an id")
  (insert proxy vo (idconv vo) :put (change-data->bytes vo (stringify-keys options))))


(defn update [proxy vo id options]
  (insert proxy vo (idconv (conj vo {:id id})) :update
          (change-data->bytes vo (stringify-keys options))))


(defn delete
  [proxy vo options]
  (insert proxy vo (idconv vo) :delete nil))


(defn append-to
  [proxy vo path path-vars value options]
  {:pre [(or (pathops/append-to-vo vo (pathops/fill-path path path-vars) value) true)]}
  (insert proxy vo (idconv vo) :append-to
          (change-data->bytes (prepare-path vo path) path-vars value (stringify-keys options))))


(defn insert-at
  [proxy vo path path-vars value options]
  {:pre [(or (pathops/insert-at vo (pathops/fill-path path path-vars) value) true)]}
  (insert proxy vo (idconv vo) :insert-at
          (change-data->bytes (prepare-path vo path) path-vars value (stringify-keys options))))


(defn move-to
  [proxy vo path path-vars to options]
  {:pre [(or (pathops/move-vo-to vo (pathops/fill-path path path-vars) to) true)]}
  (insert proxy vo (idconv vo) :move-to
          (change-data->bytes (prepare-path vo path) path-vars to (stringify-keys options))))


(defn replace-at
  [proxy vo path path-vars value options]
  {:pre [(or (pathops/replace-at vo (pathops/fill-path path path-vars) value) true)]}
  (insert proxy vo (idconv vo) :replace-at
          (change-data->bytes (prepare-path vo path) path-vars value (stringify-keys options))))


(defn merge-at
  [proxy vo path path-vars value options]
  {:pre [(or (pathops/merge-at vo (pathops/fill-path path path-vars) value) true)]}
  (let [path-value (if (and (:allow-nil-or-empty-path? options) (not (seq path))) [] path)]
    (when-not path-value
      (throw (IllegalArgumentException. (str "Empty or nil path in merge-at not allowed. "
                                             "Use :allow-nil-or-empty-path? option to override."))))
    (insert proxy vo (idconv vo) :merge-at
            (change-data->bytes (prepare-path vo path-value) path-vars value
                                (stringify-keys options)))))


(defn remove-from
  [proxy vo path path-vars options]
  {:pre [(or (pathops/remove-from vo (pathops/fill-path path path-vars)) true)]}
  (insert proxy vo (idconv vo) :remove-from
          (change-data->bytes (prepare-path vo path) path-vars (stringify-keys options))))
