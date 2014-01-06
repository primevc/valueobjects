;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defproject prime/filerepository-cassandra "0.1.0-SNAPSHOT"
  :description "The Cassandra implementation of FileRepository."
  :url "https://github.com/primevc/valueobjects"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [prime/filerepository-core "0.1.0-SNAPSHOT"]
                 [containium "0.1.0-SNAPSHOT"] ;---TODO: Use containium-cassandra when available.
                 [org.xerial.snappy/snappy-java "1.1.0-M4"]
                 [com.taoensso/timbre "2.6.2"]
                 [commons-codec "1.8"]]
  :pom-plugins [[com.theoryinpractise/clojure-maven-plugin "1.3.15"
                 {:extensions "true"
                  :configuration ([:sourceDirectories [:sourceDirectory "src"]]
                                  [:testSourceDirectories [:testSourceDirectory "test"]])
                  :executions ([:execution
                                [:id "aot-compile"]
                                [:phase "compile"]
                                [:configuration
                                 [:temporaryOutputDirectory "false"]
                                 [:copyDeclaredNamespaceOnly "true"]
                                 [:compileDeclaredNamespaceOnly "true"]
                                 [:namespaces
                                  ;; Include the namespaces here that need to be AOT compiled for
                                  ;; inclusion in the JAR here. For example:
                                  ;; [:namespace "prime.types.cassandra-repository"]
                                  [:namespace "prime.types.cassandra-repository"]]]
                                [:goals [:goal "compile"]]]
                               [:execution
                                [:id "non-aot-compile"]
                                [:phase "compile"]
                                [:configuration
                                 [:temporaryOutputDirectory "true"]
                                 [:copyDeclaredNamespaceOnly "false"]
                                 [:compileDeclaredNamespaceOnly "false"]
                                 [:namespaces
                                  ;; Include the namespaces here that you want to skip compiling
                                  ;; altogether. Start the namespaces with a bang. For example:
                                  ;; [:namespace "!some.namespace.to.ignore"]
                                  ]]
                                [:goals [:goal "compile"]]]
                               [:execution [:phase "test"] [:goals [:goal "test"]]])}]]
  :pom-addition [:properties [:project.build.sourceEncoding "UTF-8"]]
  :aot [prime.types.cassandra-repository])
