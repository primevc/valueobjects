;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.util.cassandra-history
  (:refer-clojure :exclude [get])
  (require [containium.systems.cassandra :as cassandra]
           [prime.vo]
           [prime.vo.pathops :as pathops]
           [prime.types]
           [prime.vo.util.json]
           [clojure.walk :refer (stringify-keys keywordize-keys)])
  (:import [prime.utils.msgpack VOPacker VOUnpacker]
           [java.io ByteArrayOutputStream ByteArrayInputStream]
           [prime.vo ValueObject ValueObjectField]
           [org.msgpack Unpacker MessagePackObject]
           [org.bson.types ObjectId]))


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
    org.msgpack.object.RawType (.asByteArray obj)
    org.msgpack.object.ShortIntegerTypeIMPL (.asInt obj)
    prime.utils.msgpack.MessagePackValueSource obj
    prime.utils.msgpack.MessagePackObjectId (.oid obj)))


(defn ->byte-array [value]
  (let [
    out (java.io.ByteArrayOutputStream.)
    msgpack (prime.utils.msgpack.VOPacker. out)
    pack (.pack msgpack value)]
    (.toByteArray out)))

(defn replace-star [path]
  (reduce #(cond (= % *) (conj %1 nil)
                 (and (map? %2) (= (val (first %2)) *)) (conj %1 {(key (first %2)) nil})
                 :else (conj %1 %2))
          [] path))

(defn VOChange->byte-array
  ([^ValueObject vo] (VOChange->byte-array vo nil))

  ([^ValueObject vo path & options]
      (let [out (java.io.ByteArrayOutputStream.)
            msgpack (doto (prime.utils.msgpack.VOPacker. out)
                      (.pack vo))]
        (when path
          (let [packed-path (replace-star (prime.vo/fields-path-seq vo path #(.id ^ValueObjectField %)))]
            ;; (prn 'PACKED-PATH packed-path)
            (.pack msgpack packed-path)))
        (doseq [option options]
          (let [option (if (and (map? option) (empty? option)) nil option)
                option (if (and (coll? option) (not (instance? ValueObject option))) (apply list option) option)]
            (.pack msgpack option)))
        (.close out)
        (.toByteArray out))))

(defn byte-array->VOChange
  [bytes vo]
  (let [bytes (.slice bytes)
        unpacker  (doto (org.msgpack.Unpacker. (org.apache.cassandra.utils.ByteBufferUtil/inputStream bytes))
                    (.setVOHelper prime.utils.msgpack.VOUnpacker$/MODULE$))
        [vosource fields-path & options] (iterator-seq (.. unpacker iterator))
        path (map (fn [^MessagePackObject item] (if (.isNil item) nil (as-clojure item)))
                  (when fields-path (.asArray fields-path)))]
    ;; (prn 'UNPACKED-PATH path)
    (cons (.. vo voCompanion (apply vosource)) (cons path options))))

(defn idconv [{:keys [id] :as vo}] {:pre [id]}
  ((second (simple-prime-type->cql-type (.. vo voManifest _id valueType keyword))) id))

(defn keyword-keys [m]
  (into (empty m) (for [[k v] m] [(keyword (.toLowerCase k)) v])))

;;--- TODO: VOAction enum definition
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


(defn build-vo [res vo]
  (loop [i 0 c nil] ; i = counter, c = current result.
    (let [row (nth res i {})]
      (prn 'ROW row)
      (let [c
        (cond
          (= (:action row) (:put actions))
            (first (byte-array->VOChange (:data row) vo))

            (= (:action row) (:update actions))
            ;;--- FIXME: Shouldn't this be a deep merge?
            (conj c (first (byte-array->VOChange (:data row) vo)))

          (= (:action row) (:delete actions))
            (empty c)

            ;;--- FIXME: Don't apply the pathops to the VO, but to the c var, for all vo-change
            ;;           functions below..
          (= (:action row) (:append-to actions))
          (let [[vo path path-vars value options] (byte-array->VOChange (:data row) vo)
                path (pathops/fill-path path (as-clojure path-vars))]
            ;;---TODO: Does this (as-clojure ..) below work when a VO should be appended?
            (pathops/append-to-vo vo path (as-clojure value)))

          (= (:action row) (:move-to actions))
          (let [[vo path path-vars to options] (byte-array->VOChange (:data row) vo)
                path (pathops/fill-path path (as-clojure path-vars))]
            (pathops/move-vo-to vo path (as-clojure to)))

          (= (:action row) (:remove-from actions))
          (let [[vo path path-vars options] (byte-array->VOChange (:data row) vo)
                path (pathops/fill-path path (as-clojure path-vars))]
            (pathops/remove-from vo path))

          (= (:action row) (:insert-at actions))
          (let [[vo path path-vars value options] (byte-array->VOChange (:data row) vo)
                path (pathops/fill-path path (as-clojure path-vars))]
            ;;---TODO: Does this (as-clojure ..) below work when a VO should be inserted?
            (pathops/insert-at vo path (as-clojure value)))

          (= (:action row) (:replace-at actions))
          (let [[vo path path-vars value options] (byte-array->VOChange (:data row) vo)
                path (pathops/fill-path path (as-clojure path-vars))]
            ;;---TODO: Does this (as-clojure ..) below work when a VO should be replaced?
            (pathops/replace-at vo path (as-clojure value)))

          (= (:action row) (:merge-at actions))
          (let [[vo path path-vars value options] (byte-array->VOChange (:data row) vo)
                options (keywordize-keys options)
                path (pathops/fill-path path (as-clojure path-vars))]
            (pathops/merge-at vo path value))

          :else c)]
        (if-not (empty? row)
          (recur (inc i) c)
          (if (empty? c) nil c))))))


;;; Proxy functions.

(defn get [{:keys [system consistency]} vo]
  ;; This should send a merge of all records since the last put.
  (let [last-put (:version (or (clojure.core/get (last-put-cache vo) (str (:id vo)))
                               (get-latest-put system consistency vo)))
        query (str "SELECT * FROM " (get-table-name vo) " WHERE id = ? "
                   (when last-put "and version >= ?;"))
        prepared (cassandra/prepare system query)
        args (if last-put [(idconv vo) last-put] [(idconv vo)])
        result (cassandra/do-prepared system prepared {:consistency consistency :keywordize? true}
                                      args)]
      (build-vo result vo)))


(defn get-slice [{:keys [system consistency]} vo {:keys [slices] :or [slices 1]}]
  ;; This should just send all records.
  (let [query (str "SELECT * FROM " (get-table-name vo) " WHERE id = ?")
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
                           [(org.apache.cassandra.utils.UUIDGen/getTimeUUID) id (actions action)
                            (when data (java.nio.ByteBuffer/wrap data))])))


(defn put [proxy vo options]
  ;; ---TODO: If action is put, save version into immutant cache.
  (assert (:id vo) "vo requires an id")
  (insert proxy vo (idconv vo) :put (VOChange->byte-array vo)))


(defn update [proxy vo id options]
  ;; ---FIXME: Do deep merge here.
  (insert proxy vo (idconv (conj vo {:id id})) :update (VOChange->byte-array vo)))


(defn delete [proxy vo options]
  (insert proxy vo (idconv vo) :delete nil))

;; ---FIXME: Don't store the VO for all vo-change functions below, only the action and action data.
(defn append-to [proxy vo path path-vars value options]
  (insert proxy vo (idconv vo) :append-to (VOChange->byte-array vo path path-vars value
                                                                (stringify-keys options))))


(defn insert-at [proxy vo path path-vars value options]
  (insert proxy vo (idconv vo) :insert-at (VOChange->byte-array vo path path-vars value
                                                                (stringify-keys options))))


(defn move-to [proxy vo path path-vars to options]
  (insert proxy vo (idconv vo) :move-to (VOChange->byte-array vo path path-vars to
                                                              (stringify-keys options))))


(defn replace-at [proxy vo path path-vars value options]
  (insert proxy vo (idconv vo) :replace-at (VOChange->byte-array vo path path-vars value
                                                                 (stringify-keys options))))


(defn merge-at [proxy vo path path-vars value options]
  (let [path-value (if (and (:allow-nil-or-empty-path? options) (not (seq path))) [] path)]
    (when-not path-value
      (throw (IllegalArgumentException. (str "Empty or nil path in merge-at not allowed. "
                                             "Use :allow-nil-or-empty-path? option to override."))))
    (insert proxy vo (idconv vo) :merge-at (VOChange->byte-array vo path-value path-vars value
                                                                 (stringify-keys options)))))


(defn remove-from [proxy vo path path-vars options]
  (insert proxy vo (idconv vo) :remove-from (VOChange->byte-array vo path path-vars
                                                                  (stringify-keys options))))
