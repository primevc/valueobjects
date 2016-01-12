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
  (:import [prime.utils.msgpack VOPacker VOUnpacker VOUnpacker$ MessagePackValueSource]
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
  [source ^ValueObject target-vo]
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
  "Return the given ID of VO converted to the correct CQL type."
  ([{:keys [id] :as vo}]
      {:pre [id]}
      (idconv vo id))
  ([vo id]
      {:pre [vo id]}
        (let [convert-fn (second (simple-prime-type->cql-type (.. (prime.vo/id-field vo) valueType keyword)))]
          (convert-fn id))))


;;; Change data serialization.

(defn- keyword?->name
  "When given value is a keyword, then its name is returned. Otherwise
  the value is returned unmodified."
  [k]
  (if (keyword? k) (name k) k))


(defn- replace-keywords
  "Replaces the keywords in the vo-change path by strings. This was
  implemented for the case where `prime.vo/fields-path-seq` returns a
  seq where the last path item has not been transformed. This happens
  when the path points to an item by value in a primitive (including
  VORef) array. For example: `(remove-from organization-vo [:members
  {:id 1}])`."
  [path]
  (map (fn [item]
         (if (map? item)
           {(keyword?->name (key (first item))) (val (first item))}
           item))
       path))


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
  (-> (prime.vo/fields-path-seq vo path #(.id ^ValueObjectField %))
      replace-star
      replace-keywords))


(defn- change-data->bytes
  "Serializes the given items to a byte-array, using a VO aware MsgPack."
  [& items]
  (let [out (ByteArrayOutputStream.)
        packer (VOPacker. out false)]
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


(defn- action->int
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


(def ^:private cassandra-prepare
  (memoize cassandra/prepare))

(defn- get-latest-put
  "Returns the latest PUT action row for the given VO from the
  database."
  [system consistency vo]
  (let [table-name (get-table-name vo)
        query (str "SELECT version, action FROM " table-name " WHERE id = ? ORDER BY version DESC;")
        prepared (cassandra-prepare system query)
        result (cassandra/do-prepared system prepared
                                      {:consistency consistency :keywordize? true}
                                      [(idconv vo)])]
    ;; Find first version where action == (:put actions)
    (let [put-nr (action->int :put)]
      (first (filter #(= (:action %) put-nr) result)))))

(defmacro with-debug
  "Only compiles the 'debug-expr when:
  (binding [*compiler-options* { :with-debug/history true }] ...)"
  [debug-expr & actions]
  (if (:with-debug/history *compiler-options*)
    `(do ~debug-expr
        ~@actions)
  #_else
    `(do ~@actions)))

(defn- build-vo
  "Builds a target-vo using the result rows from the database. If no
  PUT action is part of these rows, the other actions are applied on
  an empty target-vo holding only the ID."
  [result target-vo]
  (loop [rows result
         accumulator (empty target-vo)]
    (if-let [row (first rows)]
     (with-debug
      (do (prn (:version row) (:action row))
          (let [[vsrc] (bytes->change-data (:data row))] (clojure.pprint/pprint (when vsrc (msgpack/ValueSource->map vsrc))))
          (println))
      (recur (next rows)
             (case (int (:action row))
               #=(action->int :put)
               (let [vo-source (first (bytes->change-data (:data row)))]
                 (as-vo vo-source target-vo))

               #=(action->int :update)
               (let [[vo-source _] (bytes->change-data (:data row))
                     update-vo (as-vo vo-source target-vo)]
                 (utils/deep-merge-with (fn [x y] y) accumulator update-vo))

               #=(action->int :delete)
               (empty target-vo)

               #=(action->int :append-to)
               (let [[path path-vars value _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/append-to-vo accumulator filled-path value))

               #=(action->int :move-to)
               (let [[path path-vars to _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/move-vo-to accumulator filled-path to))

               #=(action->int :remove-from)
               (let [[path path-vars _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/remove-from accumulator filled-path))

               #=(action->int :insert-at)
               (let [[path path-vars value _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/insert-at accumulator filled-path value))

               #=(action->int :replace-at)
               (let [[path path-vars value _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/replace-at accumulator filled-path value))

               #=(action->int :merge-at)
               (let [[path path-vars value _] (bytes->change-data (:data row))
                     filled-path (pathops/fill-path path path-vars)]
                 (pathops/merge-at accumulator filled-path value))

              accumulator)))
      (if (empty? accumulator) nil (assoc accumulator :id (:id target-vo))))))


;;; Proxy functions.

(defn get
  "This returns a VO having all records since the last put re-applied to it."
  [{:keys [system consistency]} target-vo]
  (let [last-put (:version (get-latest-put system consistency target-vo))
        query (str "SELECT * FROM " (get-table-name target-vo) " WHERE id = ? "
                   (when last-put "and version >= ?;"))
        prepared (cassandra-prepare system query)
        args (if last-put [(idconv target-vo) last-put] [(idconv target-vo)])
        result (cassandra/do-prepared system prepared {:consistency consistency :keywordize? true}
                                      args)]
      (build-vo result target-vo)))

(defn get-ids
  "This returns all IDs of VOs ever stored in history. This includes deleted VOs."
  [{:keys [system consistency]} target-vo]
  (let [query (str "SELECT DISTINCT id FROM " (get-table-name target-vo))
        prepared (cassandra-prepare system query)
        result (cassandra/do-prepared system prepared {:consistency consistency :keywordize? true})]
      (map :id result)))


;;--- FIXME: Implement this based on dates and slice query.
;;    (and implicit search to latest put for begin date)
(defn get-slice [{:keys [system consistency]} vo {:keys [slices] :or [slices 1]}]
  ;; This should just send all records.
  #_(let [query (str "SELECT * FROM " (get-table-name vo) " WHERE id = ?")
        prepared (cassandra-prepare system query)
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
        prepared (cassandra-prepare system query)]
    (cassandra/do-prepared system prepared {:consistency consistency}
                           [(UUIDGen/getTimeUUID) id (action->int action)
                            (when data (ByteBuffer/wrap data))])))


(defn put
  [proxy vo options]
  (assert (:id vo) "vo requires an id")
  (insert proxy vo (idconv vo) :put (change-data->bytes vo)))


(defn update
  [proxy vo id options]
  (let [vo (dissoc vo (. (prime.vo/id-field vo) keyword))] ; Prevent change of the id.
    (insert proxy vo (idconv vo id) :update
            (change-data->bytes vo))))


(defn delete
  [proxy vo options]
  (insert proxy vo (idconv vo) :delete nil))


(defn append-to
  [proxy vo path path-vars value options]
  {:pre [(or (pathops/append-to-vo vo (pathops/fill-path path path-vars) value) true)]}
  (insert proxy vo (idconv vo) :append-to
          (change-data->bytes (prepare-path vo path) path-vars value)))


(defn insert-at
  [proxy vo path path-vars value options]
  {:pre [(or (pathops/insert-at vo (pathops/fill-path path path-vars) value) true)]}
  (insert proxy vo (idconv vo) :insert-at
          (change-data->bytes (prepare-path vo path) path-vars value)))


(defn move-to
  [proxy vo path path-vars to options]
  {:pre [(or (pathops/move-vo-to vo (pathops/fill-path path path-vars) to) true)]}
  (insert proxy vo (idconv vo) :move-to
          (change-data->bytes (prepare-path vo path) path-vars to)))


(defn replace-at
  [proxy vo path path-vars value options]
  {:pre [(or (pathops/replace-at vo (pathops/fill-path path path-vars) value) true)]}
  (insert proxy vo (idconv vo) :replace-at
          (change-data->bytes (prepare-path vo path) path-vars value)))


(defn merge-at
  [proxy vo path path-vars value options]
  {:pre [(or (pathops/merge-at vo (pathops/fill-path path path-vars) value) true)]}
  (let [path-value (if (and (:allow-nil-or-empty-path? options) (not (seq path))) [] path)]
    (when-not path-value
      (throw (IllegalArgumentException. (str "Empty or nil path in merge-at not allowed. "
                                             "Use :allow-nil-or-empty-path? option to override."))))
    (insert proxy vo (idconv vo) :merge-at
            (change-data->bytes (prepare-path vo path-value) path-vars value
                               ))))


(defn remove-from
  [proxy vo path path-vars options]
  {:pre [(or (pathops/remove-from vo (pathops/fill-path path path-vars)) true)]}
  (insert proxy vo (idconv vo) :remove-from
          (change-data->bytes (prepare-path vo path) path-vars)))
