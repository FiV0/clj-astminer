(defproject clj-astminer "0.1.0-SNAPSHOT"
  :description "AST mining for clojure."
  :url "https://github.com/FiV0/clj-astminer"
  :license {:name "MIT Licence"}
  :dependencies [[cheshire "5.9.0"]
                 [clj-http "3.10.0"]
                 [clj-jgit "1.0.0-beta2"]
                 ;; [com.cemerick/pomegranate "1.1.0"]
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/core.match "0.3.0"]                 
                 [org.clojure/math.combinatorics "0.1.6"]
                 ;; [org.clojure/tools.gitlibs "0.2.64"]
                 [org.clojure/tools.analyzer.jvm "0.7.2"]
                 [org.clojure/tools.namespace "0.3.1"]
                 [org.clojure/tools.trace "0.7.10"]
                 [org.clojure/tools.cli "0.4.2"]
                 [irresponsible/tentacles "0.5.1"]
                 [com.github.clojure/tools.deps.alpha "add-lib-SNAPSHOT"]]
  :repositories [["jitpack" "https://jitpack.io"]] 
  :main ^:skip-aot clj-astminer.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
