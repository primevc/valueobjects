;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defproject prime/voproxy.cassandra-history "0.1.0-SNAPSHOT"
  :description "Cassandra implementation of VOProxy."
  :url "https://github.com/primevc/valueobjects"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [prime/voproxy-core "0.1.0-SNAPSHOT"]
                 [cc.qbits/alia "1.9.2" :exclusions [org.apache.httpcomponents/httpclient]]
                 [containium "0.1.0-SNAPSHOT"]]
  :profiles {:test {:dependencies [[midje "1.5.1" :exclusions [leiningen-core]]]}}
  :pom-plugins [[com.theoryinpractise/clojure-maven-plugin "1.3.15"
                 {:extensions "true"
                  :executions ([:execution
                                [:id "clojure-compile"]
                                [:phase "compile"]
                                [:configuration
                                 [:temporaryOutputDirectory "true"]
                                 [:sourceDirectories [:sourceDirectory "src"]]]
                                [:goals [:goal "compile"]]]
                                 [:execution
                                  [:id "clojure-test"]
                                  [:phase "test"]
                                  [:goals [:goal "test"]]])}]])
