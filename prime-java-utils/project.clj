(defproject prime/utils "0.1.0-SNAPSHOT"
  :description "Clojure Utility functions for the JVM"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.taoensso/timbre "3.4.0"]
                 [commons-codec "1.10"]
                 [commons-io "2.4"]
                 [javax.mail/mail "1.4.7"]]
  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/main/java"]
  :resource-paths ["src/main/resource"]
  :test-paths ["src/test/clojure"]
  :repositories {"Sonatype" "https://repository.sonatype.org/content/repositories/forge/"
                 "Maven" "http://repo1.maven.org/maven2/org/"
                 "Clojars" "http://clojars.org/repo/"}
  :global-vars {*warn-on-reflection* true}
  :pom-plugins [[com.theoryinpractise/clojure-maven-plugin "1.7.1"
                 {:extensions "true"
                  :configuration [:sourceDirectories
                                  [:sourceDirectory "src/main/clojure"]]
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
                                  [:shortRevisionLength "8"])}]]
  :scm {:dir ".."}
  :pom-addition [:properties [:project.build.sourceEncoding "UTF-8"]])
