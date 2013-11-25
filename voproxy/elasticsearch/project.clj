;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defproject prime/voproxy-elastic "0.1.0-SNAPSHOT"
  :description "ElasticSearch VOProxy implementation."
  :url "https://github.com/primevc/valueobjects"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [prime/voproxy-core "0.1.0-SNAPSHOT"]
                 [org.elasticsearch/elasticsearch "0.90.5"]
                 [org.clojars.touch/clj-elasticsearch "0.4.1"]
                 [cheshire "5.2.0"]])
