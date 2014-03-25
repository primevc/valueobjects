;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.proxy.cassandra-history
  "A concrete implementation of the VOProxy and VOHistoryProxy
  protocols, having Cassandra as its backend."
  (:require [prime.vo.proxy :refer (VOProxy VOHistoryProxy)]
            [prime.vo.util.cassandra-history :as ch]
            [containium.systems.cassandra :as cassandra :refer (Cassandra)]
            [qbits.hayt :as hayt]))


;;; A Cassandra history proxy using the Cassandra system from containium.

(defrecord CassandraHistoryVOProxy [system consistency keyspace]
  VOProxy
  (get-vo [this vo]
    (ch/get this vo))

  (put-vo [this vo]
    (ch/put this vo {}))

  (put-vo [this vo options]
    (ch/put this vo options))

  (update [this vo]
    (ch/update this vo (:id vo) {}))

  (update [this vo id]
    (ch/update this vo id {}))

  (update [this vo id options]
    (ch/update this vo id options))

  (delete [this vo]
    (ch/delete this vo {}))

  (delete [this vo options]
    (ch/delete this vo options))

  (append-to [this vo path path-vars value]
    (ch/append-to this vo path (or path-vars ()) value {}))

  (append-to [this vo path path-vars value options]
    (ch/append-to this vo path (or path-vars ()) value options))

  (insert-at [this vo path path-vars value]
    (ch/insert-at this vo path (or path-vars ()) value {}))

  (insert-at [this vo path path-vars value options]
    (ch/insert-at this vo path (or path-vars ()) value options))

  (move-to [this vo path path-vars to]
    (ch/move-to this vo path (or path-vars ()) to {}))

  (move-to [this vo path path-vars to options]
    (ch/move-to this vo path (or path-vars ()) to options))

  (replace-at [this vo path path-vars value]
    (ch/replace-at this vo path (or path-vars ()) value {}))

  (replace-at [this vo path path-vars value options]
    (ch/replace-at this vo path (or path-vars ()) value options))

  (merge-at [this vo path path-vars value]
    (ch/merge-at this vo path (or path-vars ()) value {}))

  (merge-at [this vo path path-vars value options]
    (ch/merge-at this vo path (or path-vars ()) value options))

  (remove-from [this vo path path-vars]
    (ch/remove-from this vo path (or path-vars ()) {}))

  (remove-from [this vo path path-vars options]
    (ch/remove-from this vo path (or path-vars ()) options))

  VOHistoryProxy
  (get-slice [this vo]
    (ch/get-slice this vo {}))

  (get-slice [this vo options]
    (ch/get-slice this vo options)))


;;; Helper functions.

;; ---TODO: Have a table-exists? function for the Cassandra protocol?
(defn- table-exists?
  "Checks whether a table exists within the given keyspace, using the
  given Cassandra containium system."
  [system keyspace table]
  (let [query (str "SELECT columnfamily_name FROM system.schema_columnfamilies WHERE "
                   "keyspace_name = ? AND columnfamily_name = ?")
        prepared (cassandra/prepare system query)]
    (not (empty? (cassandra/do-prepared system prepared {:consistency :one} [keyspace table])))))


;;; Initialisation and setup functions.

(defn add-vo-support
  "Adds support for using the specified VOs in the given Cassandra
  history VOProxy. If a table already exists, it is not recreated."
  [proxy & vos]
  (doseq [vo vos]
    (let [table-name (ch/get-table-name vo)]
      (when-not (table-exists? (:system proxy) (:keyspace proxy) table-name)
        (let [id-cql-type (->> (.. (prime.vo/id-field vo) valueType keyword)
                               ch/simple-prime-type->cql-type
                               first)
              vo-columns (hayt/column-definitions {:version :timeuuid, :id id-cql-type, :action :int,
                                                   :data :blob, :primary-key [:id :version :action]})
              vo-table (hayt/create-table table-name vo-columns (hayt/with {:compact-storage true}))]
          (cassandra/write-schema (:system proxy) (hayt/->raw vo-table)))))))


(defn mk-cassandra-proxy
  "Creates a Cassandra history VOProxy. It needs a Cassandra containium
  system, and the name of the keyspace where the VO history is stored.
  If this keyspace does not exist yet, it is created. It also needs a
  consistency; possible values are documented with the Cassandra
  containium system.

  Optionally, one can supply initial VOs that need to be supported in
  this proxy. One can also add such support later using the
  `add-vo-support` function."
  [system keyspace consistency & vos]
  (when-not (cassandra/has-keyspace? system keyspace)
    (cassandra/write-schema system (str "CREATE KEYSPACE " keyspace " WITH REPLICATION = "
                              "{ 'class' : 'SimpleStrategy', 'replication_factor' : 3 };")))
  (let [session (cassandra/keyspaced system keyspace)
        proxy (CassandraHistoryVOProxy. session consistency keyspace)]
    (apply add-vo-support proxy vos)
    proxy))


;; ---TODO: Longterm: Move this kind of setup code outside of the code, i.e. make proper migrations.
