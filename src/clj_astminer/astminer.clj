(ns clj-astminer.astminer
  (:require [clojure.math.combinatorics :as combo]
            [clojure.tools.trace :refer [trace trace-ns untrace-ns]]
            ;; [clojure.core.match :refer [match]] 
            [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.jvm :as ana]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as t])
  (:gen-class))

;; This file tries to use the same naming conventions as in
;; the code2vec paper. https://code2vec.org/
;;
;; There are a few differences when compared to the original
;; paper. We use all paths of the AST where the nodes have a
;; value, meaning an AST-PATH is not necessarily a path
;; between terminal nodes. PATH-CONTEXT is extended in a
;; similar fasion. When considering an AST-PATH from a terminal
;; node to a node on the path to the root of the AST we always
;; consider this as a down path (arrow downwards).

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

(defn extend-path-extensions
  "Extends all paths extension with the given operator."
  [op paths]
  (assert (every? #(contains? % :path) paths))
  (map #(update % :path (fn [p] (cons op p))) paths))

(defn combine-path-extensions
  "Combines all path extensions from different children to paths
  with the given operator."
  [op children-path-extensions]
  ;; TODO check in which order combinations are constructed
  (->> (combo/combinations children-path-extensions 2)
       (mapcat #(apply combo/cartesian-product %))
       (map #(let [path1 (first %) path2 (second %)]
               {:path1 (:path path1) :val1 (:val path1)
                :op op
                :path2 (:path path2) :val2 (:val path2)}))))

(defn combine-path-extensions-direct
  "Creates paths from operator value and all child path extensions."
  [op value children-path-extensions]
  (->> (apply concat children-path-extensions)
       (map (fn [path-ext] {:path1 nil :val1 value
                            :op op
                            :path2 (:path path-ext) :val2 (:val path-ext)}))))

(defn create-ast-paths
  "Creates all ast-paths for a given ast transformed by transform-ast.

  The function returns a vector pair where the first value is a list
  of possible path extensions that end in this node or its children.
  A path extension is a map of the form {:path (op1 ....) :val endvalue}.

  The second value are all paths that go through this node or its children.
  A path is a map of the form
  {:path1 (op_1_1 ...) :val1 endvalue1 :op op :path2 (op_2_1 ...) :val2 endvalue2}.
  A path is essentially the combination of two path extensions and the operation
  of the LCA of the two endvalues in the AST. In case of a path from a direct
  parent to an endvalue the :path1 value will be nil."
  [ast]
  (assert (contains? ast :op))
  (cond
    (and (contains? ast :val) (contains? ast :children))
    (let [children-results (map create-ast-paths (:children ast))
          children-path-extensions (map first children-results)
          children-results (mapcat second children-results)
          {:keys [op val]} ast]
      [(cons {:path (list op) :val val}
             (extend-path-extensions op (apply concat children-path-extensions)))
       (concat (combine-path-extensions-direct op val children-path-extensions)
               (combine-path-extensions op children-path-extensions)
               children-results)])
    (contains? ast :children)
    (let* [children-results (map create-ast-paths (:children ast))
           children-path-extensions (map first children-results)
           children-results (mapcat second children-results)]
      [(extend-path-extensions (:op ast) (apply concat children-path-extensions ))
       (concat (combine-path-extensions (:op ast) children-path-extensions)
               children-results)])
    ;; TODO check if node exists that only contains 
    :else  [(list {:path (list (:op ast)) :val (:val ast)}) '()]))

(defn filter-for-defs [asts]
  (filter #(= (:op %) :def) asts))

(defn parse-file [file]
  (->> (read-string-as-clj-exprs (slurp file))
       (map ana/analyze)
       (map transform-ast)))

(defn parse-string [string]
  (->> (read-string-as-clj-exprs string)
       (map ana/analyze)
       (map transform-ast)))

;; (parse-file "resources/test.clj")

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
