;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defproject prime/voserializer-cheshire "0.1.0-SNAPSHOT"
  :description "A serializer for VOs using the Cheshire JSON library."
  :url "https://github.com/primevc/valueobjects"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [prime/vo "0.1.0-SNAPSHOT"]
                 [cheshire "5.2.0"]]
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
