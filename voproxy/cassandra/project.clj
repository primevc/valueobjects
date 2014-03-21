;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defproject prime/voproxy.cassandra-history "0.1.0-SNAPSHOT"
  :description "Cassandra implementation of VOProxy."
  :url "https://github.com/primevc/valueobjects"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [prime/vo "0.1.0-SNAPSHOT"]
                 [prime/voproxy-core "0.1.0-SNAPSHOT"]
                 [prime/voserializer-cheshire "0.1.0-SNAPSHOT"]
                 [cc.qbits/hayt "1.4.0"]
                 [containium "0.1.0-SNAPSHOT"]] ;---TODO: Use containium-cassandra when available.
  :global-vars {*warn-on-reflection* true}
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
                                [:goals [:goal "test"]]])}]]
  :pom-addition [:properties [:project.build.sourceEncoding "UTF-8"]])
