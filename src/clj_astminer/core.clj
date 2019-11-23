(ns clj-astminer.core
  (:require [clj-astminer.astminer :refer [parse-file]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io])
  (:gen-class))

(def cli-options
  [["-f" "--file FILE" "File to parse and analyze"
    :default "resources/test.clj"]
   ["-t" "--type TYPE" "Type of output"
    :default "AST"
    :validate [#(contains? ["AST" "AST-PATH" "AST-PATH-HASHED"])
               "TYPE must be one of AST AST-PATH or AST-PATH-HASHED"]]
   ["-o" "--output OUTPUT-FILE" "File to output the results to."
    :default nil]
   ["-h" "--help"]])

(defn -main
  "Main entry point for clj-astminer. Currently "
  [& args]
  (let* [args (parse-opts args cli-options)
         file (->> args :options :file)]
    (if (.exists (io/file file))
      (doall (map println (parse-file file)))
      (println "File " file " does not exist!"))))
