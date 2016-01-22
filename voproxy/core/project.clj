;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defproject prime/voproxy-core "0.1.0-SNAPSHOT"
  :description "VOProxy core library."
  :url "https://github.com/primevc/valueobjects"
  :dependencies [[prime/vo "0.1.0-SNAPSHOT" :exclusions [org.clojure/clojure]]
                 [prime/utils "0.1.0-SNAPSHOT"]
                 [org.clojure/clojure "1.8.0"]
                 [fast-zip "0.3.0"]
                 [com.taoensso/timbre "3.2.1"]]
  :profiles {:test {:dependencies [[midje "1.5.1"]]}}
  :scm {:dir "../.."}
  :pom-plugins [[com.theoryinpractise/clojure-maven-plugin "1.7.1"
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
  :pom-addition [:properties [:project.build.sourceEncoding "UTF-8"]]
  :plugins [[lein-midje-doc "0.0.17"]]
  :documentation {:files {"voproxy-doc" {:input "src/prime/vo/proxy_tests.clj"
                                         :title "VOProxy documentation"
                                         :sub-title "VOProxy documentation by tests"
                                         :author "Arnout Roemers"
                                         :email  "github@functionalbytes.nl"}}})
