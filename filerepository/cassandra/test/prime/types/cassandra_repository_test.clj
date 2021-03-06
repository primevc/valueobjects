;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.types.cassandra-repository-test
  "The test namespace for the Cassandra file repository."
  (:use [clojure.test]
        [prime.types.repository-util :refer (exists?)]
        [prime.types.cassandra-repository])
  (:require [containium.systems :refer (with-systems)]
            [containium.systems.config :as config]
            [containium.systems.cassandra :as cassandra]
            [containium.systems.cassandra.embedded :as embedded]
            [containium.systems.logging :as logging]
            [taoensso.timbre :as log]
            [prime.types.repository-util :as repo-util]
            [prime.types.cassandra-repository-util])
  (:import [java.io File]
           [org.apache.commons.io FileUtils IOUtils]
           [prime.types FileRef]))


;;; The testing fixtures.

(def cassandra (promise))

(defn cassandra-fixture
  "This wraps the tests in this namespace, and sets up an embedded Cassandra
  instance to test on."
  [f]
  (log/with-level :info
    (with-systems systems
      [:config (config/map-config {:cassandra {:config-file "cassandra-test.yaml"}})
       :logging logging/logger
       :cassandra embedded/embedded]
      (deliver cassandra (:cassandra systems))
      (try (cassandra/write-schema @cassandra "DROP KEYSPACE fs;") (catch Exception ex))
      (f))))


(defn mock-exists
  "The redefined exists call does not use Storm."
  [f]
  (with-redefs [prime.types.repository-util/exists?
                (fn [repo ^FileRef ref] (.existsImpl repo ref))]
    (f)))


(use-fixtures :once cassandra-fixture mock-exists)


;;; The actual tests.

(deftest absorb-test
  (testing "absorbing a file"

    (let [repo (cassandra-repository @cassandra :one "not-used-atm" :ttl-fn (constantly nil))
          file (File/createTempFile "cassandra" ".test")]
      (FileUtils/writeStringToFile file "cassandra test")

      (let [ref (absorb repo file)]
        (is ref "absorbing succeeds")

        (is (= (.prefixedString ref) "cassandra://2-Ll2ZG1O9D2DuVM4-8y_Oo8UMjn66zGw8OdMwUEngY")
            "it returns the correct reference")

        (is (exists? repo ref) "it contains the file")

        (is (.exists repo ref) "it contains the file (direct call)")

        (is (= (IOUtils/toString (stream repo ref)) "cassandra test")
            "it can stream the contents")

        (.delete repo ref)
        (is (not (exists? repo ref)) "it can delete the file")))))

(deftest timeout-test
  (testing "absorbing a file"

    (let [repo (cassandra-repository @cassandra :one "not-used-atm" :ttl-fn (constantly 5))
          file (File/createTempFile "cassandra" ".test")]
      (FileUtils/writeStringToFile file "cassandra test")

      (let [ref (absorb repo file)]
        (is ref "absorbing succeeds")

        (is (= (.prefixedString ref) "cassandra://2-Ll2ZG1O9D2DuVM4-8y_Oo8UMjn66zGw8OdMwUEngY")
            "it returns the correct reference")

        (is (exists? repo ref) "it contains the file")

        (Thread/sleep 5000)

        (is (not (exists? repo ref)) "it garbage collected the file")))))


(deftest store-test
  (testing "storing a file using a function"

    (let [repo (cassandra-repository @cassandra :one "not-used-atm")]

      (let [store-fn (fn [file-ref-os] (IOUtils/write "hi there!" file-ref-os))
            ref (store repo store-fn)]
        (is ref "storing succeeds")

        (is (= (.prefixedString ref) "cassandra://PjbTYi9a2tAQgMwhILtywHFOzsYRjrlSNYZBC3Q1roA")
            "it returns the correct reference")

        (is (exists? repo ref) "it contains the file")

        (is (= (IOUtils/toString (stream repo ref)) "hi there!")
            "it can stream the contents")

        (.delete repo ref)
        (is (not (exists? repo ref)) "it can delete the file")))))


(deftest mk-repository-test
  (testing "creating a Cassandra repository through the mk-repository multimethod"

    (let [repo (repo-util/mk-repository "cassandra" "not-used-atm@localhost:9042")]

      (let [store-fn (fn [file-ref-os] (IOUtils/write "hi there!" file-ref-os))
            ref (store repo store-fn)]
        (is ref "storing succeeds")

        (is (= (.prefixedString ref) "cassandra://PjbTYi9a2tAQgMwhILtywHFOzsYRjrlSNYZBC3Q1roA")
            "it returns the correct reference")

        (is (exists? repo ref) "it contains the file")

        (is (= (IOUtils/toString (stream repo ref)) "hi there!")
            "it can stream the contents")

        (.delete repo ref)
        (is (not (exists? repo ref)) "it can delete the file")))))

(deftest get-file-test
  (testing "retrieving a file via get-file"

    (let [repo (cassandra-repository @cassandra :one "not-used-atm")]

      (let [store-fn (fn [file-ref-os] (IOUtils/write "hi there!" file-ref-os))
            ref (store repo store-fn)]
        (is ref "storing succeeds")

        (is (= (.prefixedString ref) "cassandra://PjbTYi9a2tAQgMwhILtywHFOzsYRjrlSNYZBC3Q1roA")
            "it returns the correct reference")

        (is (exists? repo ref) "it contains the file")

        (is (= (FileUtils/readFileToString (get-file repo ref)) "hi there!")
            "it can return the contents as a File")

        (.delete repo ref)
        (is (not (exists? repo ref)) "it can delete the file")))))
