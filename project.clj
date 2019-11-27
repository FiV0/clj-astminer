(defproject clj-astminer "0.1.0-SNAPSHOT"
  :description "AST mining for clojure."
  :url "https://github.com/FiV0/clj-astminer"
  :license {:name "MIT Licence"}
  :dependencies [[clj-jgit "1.0.0-beta2"]
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 ;; [org.clojure/core.match "0.3.0"]                 
                 [org.clojure/tools.analyzer.jvm "0.7.2"]
                 [org.clojure/tools.trace "0.7.10"]
                 [org.clojure/tools.cli "0.4.2"]
                 [tentacles "0.5.1"]]
  :main ^:skip-aot clj-astminer.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
