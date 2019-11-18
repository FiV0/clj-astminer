(ns clj-astminer.core
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(def cli-options
  [["-r" "--repo FILE" "File to parse and analyze"
    :default "resources/test.clj"]
   ["-h" "--help"]])

(defn -main
  "Main entry point for clj-astminer."
  [& args]
  (let [args (parse-opts args cli-options)]
    nil))
