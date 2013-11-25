;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defproject prime/filerepository-nfs "0.1.0-SNAPSHOT"
  :description "An NFS implementation of FileRepository."
  :url "https://github.com/primevc/valueobjects"
  :dependencies [[org.scala-lang/scala-library "2.9.2"]
                 [prime/filerepository-core "0.1.0-SNAPSHOT"]]
  :pom-plugins [[net.alchim31.maven/scala-maven-plugin "3.1.6"
                 {:executions [:execution [:goals [:goal "compile"] [:goal "testCompile"]]]
                  :configuration ([:scalaVersion "2.9.2"]
                                  [:sourceDir "src/scala"]
                                  [:testSourceDir "test/scala"]
                                  [:recompileMode "modified-only"]
                                  [:args [:arg "-Xelide-below"] [:arg "FINEST"]
                                         [:arg "-deprecation"] [:arg "-unchecked"]
                                         [:arg "-explaintypes"]]
                                  [:jvmArgs [:arg "-XX:MaxPermSize=1024M"] [:arg "-Xmx4G"]])}]])
