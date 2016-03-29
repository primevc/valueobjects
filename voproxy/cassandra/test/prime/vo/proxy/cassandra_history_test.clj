;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.proxy.cassandra-history-test
  (:refer-clojure :exclude [proxy])
  (:require [clojure.test :refer :all]
            [containium.systems :refer (with-systems)]
            [containium.systems.config :as config]
            [containium.systems.cassandra.embedded :as embedded]
            [containium.systems.logging :as logging]
            [prime.vo.proxy :as proxy]
            [prime.vo.proxy-tests :as tests]
            [prime.vo.proxy.cassandra-history :as cassandra-proxy]
            [prime.test.vo])
  (:refer prime.test.vo))


;;; Setup embedded Cassandra and create test proxy.

(def proxy (promise))

(defn systems-fixture
  [f]
  (with-systems systems [:config (config/map-config {:cassandra {:config-file "cassandra-test.yaml"}})
                         :logging logging/logger
                         :cassandra embedded/embedded]
    (deliver proxy (cassandra-proxy/mk-cassandra-proxy
                    (:cassandra systems) "voproxytest" :one
                    (ValueObjectTest {})
                    (IntIDObjectTest {})))
    (f)))


(use-fixtures :once systems-fixture)


;;; The tests.

(deftest generic-test
  (tests/test-voproxy @proxy)
  (println "\n\nGENERIC TESTS DONE\n\n"))
