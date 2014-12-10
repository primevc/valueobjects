;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defproject prime/voproxy-elastic "0.1.0-SNAPSHOT"
  :description "ElasticSearch VOProxy implementation."
  :url "https://github.com/primevc/valueobjects"
  :dependencies [[prime/voproxy-core "0.1.0-SNAPSHOT"]
                 [prime/voserializer-cheshire "0.1.0-SNAPSHOT"]
                 [org.elasticsearch/elasticsearch "1.2.1"]
                 [clojurewerkz/elastisch "2.1.0-beta4"]
                 [clj-tuple "0.1.5"]]
  :profiles {:test {:dependencies [[containium "0.1.0-SNAPSHOT"]
                                   [org.clojars.touch/elasticsearch-lang-clojure "0.2.0-SNAPSHOT"]]}
             :mvel {:dependencies [[org.mvel/mvel2 "2.1.3.Final"]]}}
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
  :pom-addition [:properties [:project.build.sourceEncoding "UTF-8"]]
  :aliases {;; Most crappy shell ever. I mean, no history?
            "mvel-shell" ["with-profile" "+mvel" "exec" "-ep"
                          "(do (import 'org.mvel2.sh.Main)
                               (org.mvel2.sh.Main/main (into-array String [])))"]
            ;;---TODO Can I pass arguments? Now it's fixed to run script.mvel.
            "mvel-script" ["with-profile" "+mvel" "exec" "-ep"
                           "(do (import 'org.mvel2.sh.Main)
                                (org.mvel2.sh.Main/main (into-array [\"script.mvel\"])))"]})
