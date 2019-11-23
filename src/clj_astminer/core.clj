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
   ["-o" "--output OUTPUT-FILE" "File to output the results to. Can be read back "
    :default nil]
   ["-h" "--help"]])

(defn save-forms
  "Save a clojure forms to file."
  [#^java.io.File file forms]
  (with-open [w (java.io.FileWriter. file)]
    (binding [*out* w
              *print-dup* true]
      (dorun (map #(prn %) forms)))))

(defn load-forms
  "Load clojure forms from file."
  [#^java.io.File file]
  (with-open [r (java.io.PushbackReader. (java.io.FileReader. file))]
    (let [end (gensym "end")]
     (loop [reader r
            res '()]
       (let [form (read reader false end)]
         (if (= form end)
           (reverse res)
           (recur reader (cons form res))))))))

(defn write-or-print
  "Writes forms to file if not nil o/w print to stdout."
  [#^java.io.File file forms]
  (if (nil? file)
    (dorun (map println forms))
    (save-forms file forms)))

(defn -main
  "Main entry point for clj-astminer. Currently "
  [& args]
  (let* [args (parse-opts args cli-options)
         file (->> args :options :file)
         output-file (->> args :options :output)]
    (if (.exists (io/file file))
      (write-or-print (io/file output-file) (parse-file file))
      (println "File " file " does not exist!"))))
