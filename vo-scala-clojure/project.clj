(defproject prime/vo "0.1.0-SNAPSHOT"
  :description "Well-defined conversion and storage of data objects; usable as data structures"
  :dependencies [[prime/utils "0.1.0-SNAPSHOT"]
                 [org.scala-lang/scala-library "2.9.2"]
                 [org.joda/joda-convert "1.8.1"]
                 [joda-time "2.9"]
                 [prime/msgpack "0.5.1" :exclusions [org.slf4j/slf4j-api org.slf4j/slf4j-log4j12]]
                 [org.mongodb/bson "2.13.3"]
                 [fast-zip "0.5.0"]]
  :plugins [[lein-scalac "0.1.0"]]
  :profiles {:test {:dependencies [[org.specs2/specs2_2.9.2 "1.11"]]}}
  :scm {:dir ".."}
  :source-paths ["src/main/clojure"]
  :scala-source-path "src/main/scala"
  :prep-tasks ["scalac"]
  :java-source-paths ["src/main/java"]
  :resource-paths ["src/main/resource"]
  :test-paths ["src/test/clojure"]
  :repositories {"Sonatype" "https://repository.sonatype.org/content/repositories/forge/"
                 "Maven" "http://repo1.maven.org/maven2/org/"
                 "Clojars" "http://clojars.org/repo/"}
  :global-vars {*warn-on-reflection* true}
  :pom-plugins [[net.alchim31.maven/scala-maven-plugin "3.1.6"
                 {:executions [:execution [:goals [:goal "compile"] [:goal "testCompile"]]]
                  :configuration ([:scalaVersion "2.9.2"]
                                  [:recompileMode "modified-only"]
                                  [:args [:arg "-Xelide-below"] [:arg "FINEST"]
                                         [:arg "-deprecation"] [:arg "-unchecked"]
                                         [:arg "-explaintypes"]]
                                  [:jvmArgs [:arg "-XX:MaxPermSize=1024M"] [:arg "-Xmx4G"]])}]

                [com.theoryinpractise/clojure-maven-plugin "1.3.23"
                 {:extensions "true"
                  :configuration [:sourceDirectories
                                  [:sourceDirectory "src/main/clojure"]
                                  [:sourceDirectory "generated-src/clojure"]]
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
                                  [:namespace "placeholder.required"]]]
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
                                 [:execution [:phase "test"] [:goals [:goal "test"]]])}]

                [com.mmakowski/maven-specs2-plugin "0.3.0"
                 {:executions [:execution [:phase "test"] [:goals [:goal "run-specs"]]]}]

                [org.codehaus.mojo/buildnumber-maven-plugin "1.2"
                 {:executions [:execution [:phase "validate"] [:goals [:goal "create"]]]
                  :configuration ([:doCheck "false"] ; Set to true to prevent packaging with local changes.
                                  [:doUpdate "false"]
                                  [:shortRevisionLength "8"])}]

                [org.apache.maven.plugins/maven-jar-plugin "2.1"
                 {:configuration [:archive
                                  [:manifest [:addDefaultImplementationEntries "true"]]
                                  [:manifestEntries [:Shared-Version "${buildNumber}"]]]}]]
  :pom-addition [:properties [:project.build.sourceEncoding "UTF-8"]])
