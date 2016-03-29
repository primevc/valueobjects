(defproject prime/test-vo "0.1.0-SNAPSHOT"
  :description "Well-defined conversion and storage of data objects; usable as data structures"
  :scm {:dir "../"}
  :dependencies [[prime/vo "0.1.0-SNAPSHOT"]]
  :plugins [
    [lein-scalac "0.1.0"]
    [lein-shell  "0.5.0"]
  ]
  :aliases {"genvo" ["shell" "haxe" "--no-traces" "-cp" "../vo-codegen-tool/src" "-lib" "prime-core" "-cp" "src/main/haxe" "-x" "TestValueObjects"]}
  :source-paths ["src/main/clojure" "generated-src/clojure" "generated-src/scala"]
  :scala-source-path "generated-src/scala"
  :prep-tasks ["genvo" "scalac"]
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
