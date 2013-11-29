;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.util.cassandra-history-test
  (:refer-clojure :exclude [proxy])
  (:require [clojure.test :refer :all]
            [containium.systems :refer (with-systems)]
            [containium.systems.config :as config]
            [containium.systems.cassandra.embedded12 :as embedded]
            [prime.vo.proxy :as proxy]
            [prime.vo.proxy.cassandra-history :as cassandra-proxy]
            [shared.vo])
  (:refer shared.vo.account))


;;; Setup embedded Cassandra and create test proxy.

(def proxy (promise))

(defn systems-fixture
  [f]
  (with-systems systems [:config (config/map-config {:cassandra {:config-file "cassandra.yaml"}})
                         :cassandra embedded/embedded12]
    (deliver proxy (cassandra-proxy/mk-cassandra-proxy
                    (:cassandra systems) "voproxytest" :one (Organization {})))
    (f)))


(use-fixtures :once systems-fixture)


;;; The tests.

(deftest insert-test
  (let [vo (Organization {:id 1 :name "test organization" :tags ["awesome" "list" "of" "tags"]})]
    (proxy/put-vo @proxy vo)
    (is (= (proxy/get-vo @proxy (Organization {:id 1})) vo))))
