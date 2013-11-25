;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.types.cassandra-repository-test
  "The test namespace for the Cassandra file repository."
  (:use [clojure.test]
        [midje.sweet]
        [prime.types.cassandra-repository])
  (:require [qbits.alia :as alia]
            [taoensso.timbre :as log])
  (:import [java.io File]
           [org.apache.commons.io FileUtils IOUtils]
           [prime.types FileRef]))


;;; Setup functions.

(defn- start-cassandra
  "Start a Cassandra instance."
  []
  (let [daemon (org.apache.cassandra.service.CassandraDaemon.)]
    (System/setProperty "cassandra.config" "file:dev-resources/cassandra.yaml")
    (System/setProperty "cassandra-foreground" "true")
    (.activate daemon)))


;;; The testing fixtures.

(def cluster (delay (alia/cluster "localhost" :port 9042)))


(defn- prepare-cassandra
  "Write the schema to the database."
  [cluster]
  (alia/with-session (alia/connect cluster)
    (try
      (alia/execute "DROP KEYSPACE fs;")
      (catch Exception ex))
    (write-schema cluster)))


(defn cassandra-fixture
  "This wraps the tests in this namespace, and sets up an embedded Cassandra
  instance to test on."
  [f]
  (let [log-level-before (:current-level @log/config)]
    (log/set-level! :info)
    (try
      (reset! consistency :one)
      (start-cassandra)
      (prepare-cassandra @cluster)
      (f)
      (finally
        (alia/shutdown @cluster)
        (log/set-level! log-level-before)))))


(defn mock-exists
  "The redefined exists call does not use Storm."
  [f]
  (with-redefs [prime.types.cassandra-repository/exists
                (fn [repo ^FileRef ref] (.existsImpl repo ref))]
    (f)))


(use-fixtures :once cassandra-fixture mock-exists)


;;; The actual tests.

(deftest absorb-test
  (facts "about absorbing a file"

    (let [repo (cassandra-repository @cluster "not-used-atm")
          file (File/createTempFile "cassandra" ".test")]
      (FileUtils/writeStringToFile file "cassandra test")

      (fact "it succeeds"
        (let [ref (absorb repo file)]
          ref => truthy

          (fact "it returns the correct reference"
            (str ref) => "cassandra://2-Ll2ZG1O9D2DuVM4-8y_Oo8UMjn66zGw8OdMwUEngY")

          (fact "it contains the file"
            (exists repo ref) => true)

          (fact "it can stream the contents"
                (IOUtils/toString (stream repo ref)) => "cassandra test")

          (fact "it can delete the file"
                (.delete repo ref)
                (exists repo ref) => false))))))


(deftest store-test
  (facts "about storing a file using a function"

    (let [repo (cassandra-repository @cluster "not-used-atm")]

      (fact "it succeeds"
        (let [store-fn (fn [file-ref-os] (IOUtils/write "hi there!" file-ref-os))
              ref (store repo store-fn)]
          ref => truthy

          (fact "it returns the correct reference"
            (str ref) => "cassandra://PjbTYi9a2tAQgMwhILtywHFOzsYRjrlSNYZBC3Q1roA")

          (fact "it contains the file"
            (exists repo ref) => true)

          (fact "it can stream the contents"
                (IOUtils/toString (stream repo ref)) => "hi there!")

          (fact "it can delete the file"
                (.delete repo ref)
                (exists repo ref) => false))))))
