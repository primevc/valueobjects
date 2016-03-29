;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.proxy.elasticsearch-test
  (:refer-clojure :exclude [proxy])
  (:require [clojure.test :refer :all]
            [containium.systems :refer (with-systems)]
            [containium.systems.config :as config]
            [containium.systems.elasticsearch :as elastic]
            [containium.systems.logging :as logging]
            [prime.vo.util.elasticsearch :as es]
            [prime.vo.proxy :refer :all]
            [prime.vo.proxy-tests :as tests]
            [prime.vo.proxy.elasticsearch :as es-proxy])
  (:refer prime.test.vo)
  (:import [org.bson.types ObjectId]))


;;; Set up embedded ElasticSearch and create test proxy.

(def proxy (promise))

(defn systems-fixture
  [f]
  (with-systems systems [:config (config/map-config {:elastic {}})
                         :logging logging/logger
                         :elastic elastic/embedded]
    (let [es (elastic/node (:elastic systems))
          _  (es/create-index (.client es) "test-index"
                              { (ValueObjectTest {}) {:exclude #{:r}}, (IntIDObjectTest {}) {} })
          es (es-proxy/->ElasticSearchVOProxy "test-index" es {:refresh-at-change true})]
      (deliver proxy es))
    (f)
    (doseq [f  (file-seq (clojure.java.io/as-file "target/elasticsearch"))] (.delete f))))


(use-fixtures :once systems-fixture)


;;; The tests.

(deftest generic-test
  (tests/test-voproxy @proxy)
  (println "\n\nGENERIC TESTS DONE\n\n"))

(deftest elastic-test
  (when (satisfies? VOSearchProxy @proxy)
    (let [id1 (ObjectId.)
          vo1 (ValueObjectTest {:id id1
                                :name "first"
                                :meta {:tags ["foo" "bar"]}
                                :owner (VOTestRef {:name "pwned"})})
          owner (VOTestRef {:name "Greet"})
          vo2 (ValueObjectTest {:id (ObjectId.)
                            :name "second"
                            :meta {:tags ["foo" "bar" "baz"]}
                            :owner owner})]
      (put-vo @proxy vo1)
      (put-vo @proxy vo2)

      (testing "query and filter combination (A query present would cause filter to be ignored)"
        (is (= 1 (count (search @proxy (ValueObjectTest {:meta {:tags ["baz"]}})))))
        (is (= 1 (count (search @proxy (ValueObjectTest {}) {:query
          {"bool" {
            "should" [
              {"query_string" {
                "query" "baz"
                "boost" 100
                "fields" [:meta.tags]
                "lenient" false}}]}}}))))
        (is (= 0 (count (search @proxy (ValueObjectTest {:name "first"}) {:query
          {"bool" {
            "should" [
              {"query_string" {
                "query" "baz"
                "boost" 100
                "fields" [:meta.tags]
                "lenient" false}}]}}})))))

      (delete @proxy vo1)
      (delete @proxy vo2))))
