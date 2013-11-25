;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defproject prime/voproxy.cassandra-history "0.1.0-SNAPSHOT"
  :description "Cassandra implementation of VOProxy."
  :url "https://github.com/primevc/valueobjects"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [prime/voproxy-core "0.1.0-SNAPSHOT"]
                 [org.apache.cassandra/cassandra-all "1.2.10"]]
  :profiles {:test {:dependencies [[midje "1.5.1"]
                                   [containium "0.1.0-SNAPSHOT"]]}})
