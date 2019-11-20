(ns clj-astminer.astminer
  (:require [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.jvm :as ana]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as t])
  (:gen-class))

(defn read-string-as-clj-exprs
  "Reads a string of Clojure expressions into a vector.

  An read-eval expression is not evaluated but rather parsed as is."
  ([string] (read-string-as-clj-exprs string true))
  ([string read-eval?]
   (binding [r/*read-eval* read-eval?]
     (let [end (gensym "end_")
           read-macro-err (gensym "read_macro_err_")]
       (loop [input (t/source-logging-push-back-reader string)
              res '[]]
         (let [exp (try (r/read input false end)
                        (catch clojure.lang.ExceptionInfo e read-macro-err))]
           (if (= exp end) res
               (recur input (if (= exp read-macro-err) res (conj res exp))))))))))

(defn filter-for-defs [asts]
  (filter #(= (:op %) :def) asts))

(defn parse-file [file]
  (map ana/analyze (read-string-as-clj-exprs (slurp file))))

;; (set! *print-length* 10)
;; (set! *print-level* 10)

;; (require '[clojure.tools.reader :as r])
;; (require '[clojure.tools.reader.reader-types :as t])
;; (binding [r/*read-eval* false]
;;  (loop [input (t/source-logging-push-back-reader "(+ #=(+ 1 2) 3) :foo")
;;         res '[]]
;;    (let [exp (try (r/read input false :end)
;;                   (catch clojure.lang.ExceptionInfo e :read-err))]
;;      (if (= exp :end) res
;;          (recur input (if (= exp :read-err) res (conj res exp)))))))
;; => [(+ 1 2) 3 :foo]
;; => want just [:foo]
