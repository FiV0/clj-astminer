(ns clj-astminer.astminer
  (:require [clojure.tools.analyzer.jvm :as ana]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as t])
  (:gen-class))

(defn read-string-as-clj-exprs
  "Reads a string of Clojure expressions into a vector.

  An read-eval expression is not evaluated but rather parsed as is."
  [string]
  (binding [r/*read-eval* false]
    (let [end (gensym "end_")
          read-macro-err (gensym "read_macro_err_")]
      (loop [input (t/source-logging-push-back-reader string)
             res '[]]
        (let [exp (try (r/read input false end)
                       (catch clojure.lang.ExceptionInfo e read-macro-err))]
          (if (= exp end)
            (reverse res)
            (recur input (if (= exp read-macro-err) res (conj res exp)))))))))

(defn parse-file [file]
  (read-string-as-clj-exprs (slurp file)))
