(ns clj-astminer.astminer
  (:require [clojure.tools.trace :refer [trace]]
            ;; [clojure.core.match :refer [match]] 
            [clojure.tools.analyzer.ast :as ast]
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

(defn conj-not-nil [coll val]
  (if (= val nil)
    coll
    (conj coll val)))

(defn vec-to-list
  "Transforms a vector into a list."
  [vec]
  (assert (instance? clojure.lang.APersistentVector vec))
  (reverse (into '() vec)))

(defn transform-ast
  "Transform an AST given by tools.reader.jvm/analyze into a more managable structure.
  FIXME Description"
  [ast]
  (assert (contains? ast :op))
  (case (:op ast)
    ;; TODO check if uniquified are bad or good for ML
    :binding {:op :binding :val (:form ast) 
              :children (map transform-ast (map #(% ast) (:children ast)))}
    :case {:op :case :children
           (map transform-ast (as-> (interleave (:tests ast) (:thens ast)) v
                                (cons (:test ast) v)
                                (concat v [(:default ast)])))}
    ;;NOTE ignoring metadata here
    :const {:op :const :val (:form ast)}
    :def {:op :def :val (:name ast) :doc (:doc ast)
          :children (->> (filter #(= % :meta) (:children ast))
                         (map #(% ast))
                         (map transform-ast))}
    :deftype {:op :deftype :val (:name ast)
              :children (map transform-ast (map #(% ast) (:children ast)))}
    :do {:op :do :children (->> (conj (:statements ast) (:ret ast))
                                (map transform-ast))}
    :fn {:op :fn :children (->> (:local ast)
                                (conj-not-nil (vec-to-list (:methods ast)))
                                (map transform-ast))}
    :fn-method {:op :fn-method :children (->> (conj (:params ast) (:body ast))
                                              (map transform-ast))}
    :import {:op :import :val (:class ast)}
    :instance-call {:op :instance-call :val (:method ast)
                    :children (->> (cons (:instance ast) (vec-to-list (:args ast)))
                                   (map transform-ast))}
    :instance-field {:op :instance-field :val (:field ast)
                     :children (map transform-ast (map #(% ast) (:children ast)))}
    :instance? {:op :instance? :val (:class ast)
                :children (map transform-ast (map #(% ast) (:children ast)))}
    :invoke {:op :invoke
             :children (->> (cons (:fn ast) (vec-to-list (:args ast)))
                            (map transform-ast))}
    :keyword-invoke {:op :keyword-invoke :val (second (:form ast))}
    :local {:op :local :val (:form ast)}
    :map {:op :map :children (->> (interleave (:keys ast) (:vals ast))
                                  (map transform-ast))}
    :method {:op :method :val (:name ast)
             :children (->> (vec-to-list (conj (:params ast) (:body ast)) )
                            (cons (:this ast))
                            (map transform-ast))}
    :new {:op :new :children (->> (cons (:class ast) (vec-to-list (:args ast)))
                                  (map transform-ast))}
    :primitive-invoke {:op :primitive-invoke
                       :children (->> (cons (:fn ast) (vec-to-list (:args ast)))
                                      (map transform-ast))}
    :protocol-invoke {:op :protocol-invoke
                      :children (->> (cons (:protocol-fn ast)
                                           (cons (:target ast) (vec-to-list (:args ast))))
                                     (map transform-ast))}
    :recur {:op :recur :children (map transform-ast (:exprs ast))}
    :reify {:op :reify :children (map transform-ast (:methods ast))}
    :set {:op :set :children (map transform-ast (:items ast))}
    :static-call {:op :static-call :val (str (.getName (:class ast)) "/" (:method ast)) 
                  :children (map transform-ast (:args ast))}
    :static-field {:op :static-field :val (str (.getName (:class ast)) "/" (:method ast))}
    ;; NOTE maybe use (second (:form ast)) instead of :var
    :the-var {:op :the-var :val (second (:form ast))}
    :try {:op :try :children (->> (conj-not-nil (:catches ast) (:finally ast)) 
                                  (cons (:body ast))
                                  (map transform-ast))}
    :var {:op :var :val (:form ast)}
    :vector {:op :vector :children (map transform-ast (:items ast))}
    :with-meta {:op :with-meta :children (list (transform-ast (:meta ast)))} 
    (:let :letfn :loop) {:op (:op ast)
                   :children (->> (conj (:bindings ast) (:body ast))
                                  (map transform-ast))}
    (:cast-test :cast-then :catch :host-interop :if :monitor-enter
                :monitor-exit :quote :set! :throw)
    {:op (:op ast) :children (map transform-ast (map #(% ast) (:children ast)))}
    (throw (Exception. "Undefined AST node !!!"))))

(defn filter-for-defs [asts]
  (filter #(= (:op %) :def) asts))

(defn parse-file [file]
  (->> (read-string-as-clj-exprs (slurp file))
       (map ana/analyze)
       (map transform-ast)))

(defn parse-string [string]
  (map ana/analyze (read-string-as-clj-exprs string)))


;; (parse-file "resources/test.clj")
;; (count *1) 
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
