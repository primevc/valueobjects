;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defproject prime/filerepository-cassandra "0.1.0-SNAPSHOT"
  :description "The Cassandra implementation of FileRepository."
  :url "https://github.com/primevc/valueobjects"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [prime/filerepository-core "0.1.0-SNAPSHOT"]
                 [cc.qbits/alia "1.9.1" :exclusions [org.apache.httpcomponents/httpclient]]
                 [com.taoensso/timbre "2.6.2"]
                 [commons-codec "1.8"]]
  :profiles {:test {:dependencies [[midje "1.5.1"]]}}
  :pom-plugins [[com.theoryinpractise/clojure-maven-plugin "1.3.15"
                 {:extensions "true"
                  :configuration ([:sourceDirectories [:sourceDirectory "src"]]
                                  [:testSourceDirectories [:testSourceDirectory "test"]]
                                  [:temporaryOutputDirectory "false"])
                  :executions ([:execution [:id "clojure-compile"] [:phase "compile"]
                                [:goals [:goal "compile"]]]
                               [:execution [:id "clojure-test"] [:phase "test"]
                                [:goals [:goal "test"]]])}]]
  :pom-addition [:properties [:project.build.sourceEncoding "UTF-8"]])
