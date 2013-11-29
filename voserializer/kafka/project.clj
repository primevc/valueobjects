;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defproject prime/voserializer-kafka "0.1.0-SNAPSHOT"
  :description "A Kafka encoder/decoder for ValueObjects."
  :url "https://github.com/primevc/valueobjects"
  :dependencies [[org.scala-lang/scala-library "2.9.2"]
                 [prime/vo "0.1.0-SNAPSHOT"]
                 [kafka/core-kafka_2.9.2 "0.7.2"]]
  :profiles {:test {:dependencies [[org.specs2/specs2_2.9.2 "1.11"]]}}
  :pom-plugins [[net.alchim31.maven/scala-maven-plugin "3.1.6"
                 {:executions [:execution [:goals [:goal "compile"] [:goal "testCompile"]]]
                  :configuration ([:scalaVersion "2.9.2"]
                                  [:recompileMode "modified-only"]
                                  [:args [:arg "-Xelide-below"] [:arg "FINEST"]
                                         [:arg "-deprecation"] [:arg "-unchecked"]
                                         [:arg "-explaintypes"]]
                                  [:jvmArgs [:arg "-XX:MaxPermSize=1024M"] [:arg "-Xmx4G"]])}]

                [com.mmakowski/maven-specs2-plugin "0.3.0"
                 {:executions [:execution [:phase "test"] [:goals [:goal "run-specs"]]]}]]
  :pom-addition [:properties [:project.build.sourceEncoding "UTF-8"]])
