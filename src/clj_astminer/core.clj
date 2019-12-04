(ns clj-astminer.core
  (:require [clj-astminer.astminer :refer
             [file-to-asts file-to-ast-paths file-to-code2vec
              clojar-name-to-asts clojar-name-to-ast-paths
              clojar-name-to-code2vec]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io])
  (:gen-class))

(defn in? 
  "True if coll contains elm."
  [coll elm]  
  (some #(= elm %) coll))

(def cli-options
  [["-f" "--file FILE" "File to parse and analyze."
    :default "resources/test.clj"]
   ["-p" "--project PROJECT-NAME" "Project to retrieve from clojars."
    :default nil]
   ["-t" "--type TYPE" "Type of output."
    :default "AST"
    :validate [#(in? ["AST" "AST-PATH" "AST-PATH-HASHED"] %)
               "TYPE must be one of AST AST-PATH or AST-PATH-HASHED"]
    ]
   ["-o" "--output OUTPUT-FILE" "File to output the results to."
    :default nil]
   ["-h" "--help"]])

(defmethod print-dup clojure.lang.Atom [o w]
  (print-ctor o (fn [o w] (print-dup (deref o) w)) w))

(defn save-forms
  "Save clojure forms to file."
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
  (let [args (parse-opts args cli-options)
        options (:options args) 
        file (:file options)
        project-name (:project options)
        output-file (:output options)
        type (:type options)]
    ;; TODO add error checking
    (if (nil? project-name)
      (case type
        "AST" (write-or-print (io/file output-file) (file-to-asts file))
        "AST-PATH" (write-or-print (io/file output-file) (file-to-ast-paths file))
        "AST-PATH-HASHED" (write-or-print (io/file output-file) (file-to-code2vec file)) 
        (throw (Exception. "Should not happen!!!")))
      (if (.exists (io/file file)) 
        (case type
          "AST" (write-or-print (io/file output-file) (clojar-name-to-asts project-name))
          "AST-PATH" (write-or-print (io/file output-file) (clojar-name-to-ast-paths project-name))
          "AST-PATH-HASHED" (write-or-print (io/file output-file) (clojar-name-to-code2vec project-name)) 
          (throw (Exception. "Should not happen!!!")))
        (println "File " file " does not exist!")))))

(comment
  (-main "-p" "leiningen" "-o" "resources/output.txt" "-t" "AST-PATH")
  )
