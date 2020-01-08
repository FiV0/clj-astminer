(ns clj-astminer.core
  (:require [clj-astminer.astminer :refer
             [file-to-asts file-to-ast-paths file-to-code2vec
              clojar-name-to-asts clojar-name-to-ast-paths
              clojar-name-to-code2vec all-clojars-to-asts
              all-clojars-to-ast-paths all-clojars-to-code2vec]]
            [clojure.core.match :refer [match]]
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
               "TYPE must be one of AST AST-PATH or AST-PATH-HASHED"]]
   ["-o" "--output OUTPUT-FILE" "File to output the results to."
    :default nil]
   ["-a" "--all" "Load all non fork projects from clojar."]
   ["-l" "--limit NUMBER" "Limit the number of clojar repos."
    :default 100
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--help"]])

(defmethod print-dup clojure.lang.Atom [o w]
  (print-ctor o (fn [o w] (print-dup (deref o) w)) w))

(defn save-forms
  "Save clojure forms to file."
  ([#^java.io.File file forms print-dup]
   (with-open [w (java.io.FileWriter. file)]
     (binding [*out* w
               *print-dup* print-dup]
       (dorun (map prn forms))))))

(defn build-code2vec-string [ls i]
  (println "cucu" i)
  #dbg ^{:break/when (>= i 111)}
  (let [res (apply str
                   (print-str (first ls) " ")
                   (reduce (fn [res [x y z]]
                             (str res
                                  (pr-str x) (print-str ",")
                                  (pr-str y) (print-str ",")
                                  (pr-str z) (print-str " ")))
                           ""
                           (rest ls)))]
    (println "cici")
    res))

(defn save-forms-code2vec
  "Save clojure forms to file."
  ([#^java.io.File file forms print-dup]
   (with-open [w (clojure.java.io/writer file)]
     ;; (dorun (map-indexed #(.write w (build-code2vec-string %2 %1)) forms))
     (dorun (map-indexed #(build-code2vec-string %2 %1) forms))
     )))

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
  ([#^java.io.File file forms print-dup] 
   (if (nil? file)
     (dorun (map println forms))
     (save-forms file forms print-dup))))

(defn write-or-print-code2vec
  ([#^java.io.File file forms print-dup] 
   (if (nil? file)
     (dorun (map #(println (build-code2vec-string %)) forms))
     (save-forms-code2vec file forms print-dup))))

(defn -main
  "Main entry point for clj-astminer. Currently "
  [& args]
  (let [cl (.getContextClassLoader (Thread/currentThread))]
    (.setContextClassLoader (Thread/currentThread) (clojure.lang.DynamicClassLoader. cl))
    (let [args (parse-opts args cli-options)
          options (:options args) 
          file (:file options)
          project-name (:project options)
          output-file (io/file (:output options))
          type (:type options)
          all (:all options)
          limit (:limit options)]
      ;; TODO add error checking
      ;; (println [all project-name (.exists (io/file file)) type])
      (match [all project-name (.exists (io/file file)) type]
             [true _ _ "AST"] (write-or-print output-file (all-clojars-to-asts) true) 
             [true _ _ "AST-PATH"] (write-or-print output-file (all-clojars-to-ast-paths) true)
             [true _ _ "AST-PATH-HASHED"]
             (write-or-print-code2vec output-file (all-clojars-to-code2vec limit) false)
             [_ nil false _] (println "File " file " does not exist!")
             [_ nil true "AST"] (write-or-print output-file (file-to-asts file) true) 
             [_ nil true "AST-PATH"] (write-or-print output-file (file-to-ast-paths file) true) 
             [_ nil true "AST-PATH-HASHED"]
             (write-or-print-code2vec output-file (file-to-code2vec file) false) 
             [_ project-name _ "AST"]
             (write-or-print output-file (clojar-name-to-asts project-name) true)
             [_ project-name _ "AST-PATH"]
             (write-or-print output-file (clojar-name-to-ast-paths project-name) true)
             [_ project-name _ "AST-PATH-HASHED"]
             ;; (clojar-name-to-code2vec project-name false true)
             (write-or-print-code2vec output-file (clojar-name-to-code2vec project-name) false)
             :else (throw (Exception. "Should not happen!!!")))))
  )

(comment
  (-main "-p" "chu.graph" "-o" "resources/output.txt" "-t" "AST-PATH-HASHED")
  (-main "-p" "viz-cljc" "-o" "resources/output.txt" "-t" "AST-PATH-HASHED")
  (-main "-o" "resources/output.txt" "-t" "AST-PATH-HASHED")
  (-main "-t" "AST-PATH-HASHED")
  (-main "-a" "-o" "resources/output.txt" "-t" "AST-PATH-HASHED")
  (-main "-a" "-l" "2" "-o" "resources/output.txt" "-t" "AST-PATH-HASHED")
  (-main "-t" "AST" "-f" "resources/reader-conditional-")
  (clojure.lang.DynamicClassLoader)


  (try
    (-main "-p" "chu.graph" "-o" "resources/output.txt" "-t" "AST-PATH-HASHED")
    (catch Exception e (ex-data e)))

  (set! *print-level* 10)
  (set! *print-length* 10)
  )
