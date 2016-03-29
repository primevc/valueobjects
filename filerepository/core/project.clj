;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defproject prime/filerepository-core "0.1.0-SNAPSHOT"
  :description "The core FileRepository functionality."
  :url "https://github.com/primevc/valueobjects"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.scala-lang/scala-library "2.9.2"]
                 [prime/vo "0.1.0-SNAPSHOT"]]
  :profiles {:test {:dependencies [[org.specs2/specs2_2.9.2 "1.11"]]}}
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :plugins [[lein-scalac "0.1.0"]]
  :scala-source-path "src/main/scala"
  :prep-tasks ["scalac"]
  :pom-plugins [[net.alchim31.maven/scala-maven-plugin "3.1.6"
                 {:executions [:execution [:goals [:goal "compile"] [:goal "testCompile"]]]
                  :configuration ([:scalaVersion "2.9.2"]
                                  [:recompileMode "modified-only"]
                                  [:args [:arg "-Xelide-below"] [:arg "FINEST"]
                                         [:arg "-deprecation"] [:arg "-unchecked"]
                                         [:arg "-explaintypes"]]
                                  [:jvmArgs [:arg "-XX:MaxPermSize=1024M"] [:arg "-Xmx4G"]])}]

                [com.theoryinpractise/clojure-maven-plugin "1.3.15"
                 {:extensions "true"
                  :configuration ([:sourceDirectories [:sourceDirectory "src/main/clojure"]]
                                  [:testSourceDirectories [:testSourceDirectory "src/test/clojure"]]
                                  [:temporaryOutputDirectory "true"])
                  :executions [:execution [:phase "compile"] [:goals [:goal "compile"]]]}]

                [com.mmakowski/maven-specs2-plugin "0.3.0"
                 {:executions [:execution [:phase "test"] [:goals [:goal "run-specs"]]]}]]
  :pom-addition [:properties [:project.sourceEncoding "UTF-8"]])
