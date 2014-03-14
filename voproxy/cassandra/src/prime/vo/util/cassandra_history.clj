;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.util.cassandra-history
  (:refer-clojure :exclude [get])
  (require [containium.systems.cassandra :as cassandra]
           [prime.vo.pathops :as pathops]
           [prime.utils :as utils]
           [prime.utils.msgpack :as msgpack]
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


;;; Helper functions.

(defn get-table-name
  "Returns the table name for the given VO."
  [vo]
  (str "t" (Integer/toHexString (.ID (prime.vo/manifest vo)))))


(defn- as-vo
  "Returns the actual VO based on a source and a target."
  [source target-vo]
  (.. target-vo voCompanion (apply source)))


;;; Column serialization.

(defn- ObjectId->ByteBuffer
  "Wrap an ObjectId in an ByteBuffer."
  [^org.bson.types.ObjectId oid]
  (ByteBuffer/wrap (.toByteArray oid)))


(defn simple-prime-type->cql-type
  "Given a prime type keyword, returns a tuple having the equivalent
  CQL type and a conversion function to get to that type."
  [type]
  (case type
    :prime.types/ObjectId   [ :blob       ObjectId->ByteBuffer    ]
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
    :prime.types/Date+time  [ :timestamp  prime.types/to-DateTime ]))


(defn- idconv
  "Returns the ID of the given VO, converted to the correct CQL type."
  [{:keys [id] :as vo}]
  {:pre [id]}
  (let [convert-fn (second (simple-prime-type->cql-type (.. vo voManifest _id valueType keyword)))]
    (convert-fn id)))


;;; Change data serialization.

(defn- replace-star
  "Replaces the * parts in the vo-change path."
  [path]
  (reduce #(cond (= % *) (conj %1 nil)
                 (and (map? %2) (= (val (first %2)) *)) (conj %1 {(key (first %2)) nil})
                 :else (conj %1 %2))
          [] path))


(defn- prepare-path
  "Prepares a vo-change path such that it can be serialized."
  [vo path]
  (replace-star (prime.vo/fields-path-seq vo path #(.id ^ValueObjectField %))))


(defn- change-data->bytes
  "Serializes the given items to a byte-array, using a VO aware MsgPack."
  [& items]
  (let [out (ByteArrayOutputStream.)
        packer (VOPacker. out)]
    (doseq [item items] (.pack packer item))
    (.toByteArray out)))


(defn- bytes->change-data
  "Deserializes the given byte-array, using a VO-source aware MsgPack.
  The items returned are Clojure data structures and/or
  prime.utils.msgpack.MessagePackValueSource instances."
  [bytes]
  (let [unpacker (doto (Unpacker. (ByteBufferUtil/inputStream bytes))
                   (.setVOHelper VOUnpacker$/MODULE$))]
    (map msgpack/as-clojure (iterator-seq (.. unpacker iterator)))))


(defn- actions
  [key]
  (case key
    :put (int 1)
    :update (int 2)
    :delete (int 3)
    :append-to (int 5)
    :move-to (int 4)
    :remove-from (int 6)
    :insert-at (int 7)
    :replace-at (int 8)
    :merge-at (int 9)))


(defn- get-latest-put
  "Returns the latest PUT action row for the given VO from the
  database."
  [system consistency vo]
  (let [table-name (get-table-name vo)
        query (str "SELECT version, action FROM " table-name " WHERE id = ? ORDER BY version DESC;")
        prepared (cassandra/prepare system query)
        result (cassandra/do-prepared system prepared
                                      {:consistency consistency :keywordize? true}
                                      [(idconv vo)])]
    ;; Find first version where action == (:put actions)
    (let [put-nr (actions :put)]
      (first (filter #(= (:action %) put-nr) result)))))


(defn- build-vo
  "Builds a target-vo using the result rows from the database. If no
  PUT action is part of these rows, the other actions are applied on
  an empty target-vo holding only the ID."
  [result target-vo]
  (loop [index 0
         accumulator (empty target-vo)]
    (if-let [row (nth result index nil)]
      (recur (inc index)
             (condp = (:action row)
               (actions :put)
               (let [vo-source (first (bytes->change-data (:data row)))]
                 (as-vo vo-source target-vo))

               (actions :update)
               (let [[vo-source _] (bytes->change-data (:data row))
                     update-vo (as-vo vo-source target-vo)]
                 (utils/deep-merge-with (fn [x y] y) accumulator update-vo))

               (actions :delete)
               (empty target-vo)

               (actions :append-to)
               (let [[path path-vars value _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/append-to-vo accumulator filled-path value))

               (actions :move-to)
               (let [[path path-vars to _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/move-vo-to accumulator filled-path to))

               (actions :remove-from)
               (let [[path path-vars _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/remove-from accumulator filled-path))

               (actions :insert-at)
               (let [[path path-vars value _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/insert-at accumulator filled-path value))

               (actions :replace-at)
               (let [[path path-vars value _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/replace-at accumulator filled-path value))

               (actions :merge-at)
               (let [[path path-vars value _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/merge-at accumulator filled-path value))

              :else accumulator))
      (if (empty? accumulator) nil (assoc accumulator :id (:id target-vo))))))


;;; Proxy functions.

(defn get
  "This returns a VO having all records since the last put re-applied to it."
  [{:keys [system consistency]} target-vo]
  (let [last-put (:version (get-latest-put system consistency target-vo))
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
      (take slices (byte-array->VOChange (:data (last result)) vo)))
  (assert false "Not yet implemented."))


(defn- insert
  "Inserts the change data into the database."
  [{:keys [system consistency]} vo id action data]
  (let [query (str "INSERT INTO " (get-table-name vo) " (version, id, action, data) "
                   "VALUES ( ? , ? , ? , ? )")
        prepared (cassandra/prepare system query)]
    (cassandra/do-prepared system prepared {:consistency consistency}
                           [(UUIDGen/getTimeUUID) id (actions action)
                            (when data (ByteBuffer/wrap data))])))


(defn put
  [proxy vo options]
  (assert (:id vo) "vo requires an id")
  (insert proxy vo (idconv vo) :put (change-data->bytes vo (stringify-keys options))))


(defn update
  [proxy vo id options]
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
