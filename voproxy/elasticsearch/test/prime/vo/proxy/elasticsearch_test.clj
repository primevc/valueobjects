;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns prime.vo.proxy.elasticsearch-test
  (:refer-clojure :exclude [proxy])
  (:require [clojure.test :refer :all]
            [containium.systems :refer (with-systems)]
            [containium.systems.config :as config]
            [containium.systems.elasticsearch :as elastic]
            [prime.vo.proxy :as proxy]
            [prime.vo.proxy-tests :as tests]
            [prime.vo.proxy.elasticsearch]) ;; Load for the deftype.
  (:import [prime.vo.proxy.elasticsearch ElasticSearchVOProxy]))


;;; Set up embedded ElasticSearch and create test proxy.

(def proxy (promise))

(defn systems-fixture
  [f]
  (with-systems systems [:config (config/map-config {:elastic {}})
                         :elastic elastic/embedded]
    (deliver proxy (ElasticSearchVOProxy. "test-index" (elastic/node (:elastic systems))
                                          {:refresh-at-change true}))
    (f)))


(use-fixtures :once systems-fixture)


;;; The tests.

(deftest generic-test
  (tests/test-voproxy @proxy))
