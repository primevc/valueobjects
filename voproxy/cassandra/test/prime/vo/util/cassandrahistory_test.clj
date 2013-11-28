;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.util.cassandra-history-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [containium.systems :refer (with-systems)]
            [containium.systems.config :refer (map-config)]
            [containium.systems.cassandra.embedded12 :refer (embedded12)]
            [qbits.alia :as alia]
            [qbits.hayt :as hayt]
            [prime.vo.proxy.cassandra-history :as proxy]
            [shared.vo]))


(def cluster (delay (alia/cluster "192.168.33.1")))


(defn- column-definitions [vo]
  (hayt/column-definitions
   {:version     :timeuuid ; Version
    :id          (first (history/simple-prime-type->cql-type (.. vo voManifest _id valueType keyword)))
    :action      :int      ; Action
    :data        :blob     ; Vo + path.
    :primary-key [:id :version :action]}))


(defn- prepare-cassandra
  "Write the schema to the database."
  [cluster]
  (alia/with-session (alia/connect cluster)
    (try
      (alia/execute "DROP KEYSPACE test1e;")
      (catch Exception ex))
    (try
      (alia/execute "CREATE KEYSPACE test1e WITH REPLICATION = {'CLASS' : 'SimpleStrategy',
                                                                'replication_factor': 3};")
      (catch Exception ex))
    (try
      (alia/execute "USE test1e;")
      (catch Exception ex))
      (let [vo (shared.vo.account/Organization{})
            table (hayt/create-table (keyword (str "t" (Integer/toHexString (.. vo voManifest ID))))
                                     (column-definitions vo)
                                     (hayt/with {:compact-storage true}))]
        (try
          (alia/execute (hayt/->raw table))
          (catch Exception ex)))))


(deftest insert-functions-test
  (let [vo (shared.vo.account/Organization{:id 1 :name "test organization"
                                           :tags ["awesome" "list" "of" "tags"]})]

    (fact "Insert a vo, get it and the same vo should be returned."
      (do (history/put @cluster vo nil)
          (history/get @cluster (shared.vo.account/Organization{:id 1})))
      => vo)))
