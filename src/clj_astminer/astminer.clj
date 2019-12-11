(ns clj-astminer.astminer
  (:require [clj-astminer.retrieve :as retrieve]
            [clojure.math.combinatorics :as combo]
            [clojure.tools.trace :refer [trace trace-ns untrace-ns]]
            ;; [clojure.core.match :refer [match]] 
            [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.jvm :as ana]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as t]
            [clojure.string :as st])
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

(defmulti transform-ast
  "Transform an AST given by tools.reader.jvm/analyze into a more managable structure.
  FIXME Description"
  :op)

;; TODO check if uniquified are bad or good for ML
;; TODO maybe remove meta-data stuff

(defmethod transform-ast :binding [ast]
  {:op :binding :val (:form ast) 
   :children (map transform-ast (map #(% ast) (:children ast)))})

(defmethod transform-ast :case [ast]
  {:op :case :children
   (map transform-ast (as-> (interleave (:tests ast) (:thens ast)) v
                        (cons (:test ast) v)
                        (concat v [(:default ast)])))})

;; NOTE ignoring metadata here
(defmethod transform-ast :const [ast]
  {:op :const :val (:form ast)})

(defmethod transform-ast :def [ast]
  {:op :def :val (:name ast) :doc (:doc ast)
   :children (if (nil? (:init ast)) nil
                 (list (transform-ast (:init ast))))})

(defmethod transform-ast :deftype [ast]
  {:op :deftype :val (:name ast)
   :children (->> (concat (:fields ast) (:methods ast))
                  (map transform-ast))})

(defmethod transform-ast :do [ast]
  {:op :do :children (->> (conj (:statements ast) (:ret ast))
                          (map transform-ast))})

(defmethod transform-ast :fn [ast]
  {:op :fn :children (->> (:local ast)
                          (conj-not-nil (vec-to-list (:methods ast)))
                          (map transform-ast))})

(defmethod transform-ast :fn-method [ast]
  {:op :fn-method :children (->> (conj (:params ast) (:body ast))
                                 (map transform-ast))})

(defmethod transform-ast :import [ast]
  {:op :import :val (:class ast)})

(defmethod transform-ast :instance-call [ast]
  {:op :instance-call :val (:method ast)
   :children (->> (cons (:instance ast) (vec-to-list (:args ast)))
                  (map transform-ast))})

(defmethod transform-ast :instance-field [ast]
  {:op :instance-field :val (:field ast)
   :children (map transform-ast (map #(% ast) (:children ast)))})

(defmethod transform-ast :instance? [ast]
  {:op :instance? :val (:class ast)
   :children (map transform-ast (map #(% ast) (:children ast)))})

(defmethod transform-ast :invoke [ast]
  {:op :invoke
   :children (->> (cons (:fn ast) (vec-to-list (:args ast)))
                  (map transform-ast))})

(defmethod transform-ast :keyword-invoke [ast]
  {:op :keyword-invoke :val (second (:form ast))})

(defmethod transform-ast :local [ast]
  {:op :local :val (:form ast)})

(defmethod transform-ast :map [ast]
  {:op :map :children (->> (interleave (:keys ast) (:vals ast))
                           (map transform-ast))})

(defmethod transform-ast :method [ast]
  {:op :method :val (:name ast)
   :children (->> (vec-to-list (conj (:params ast) (:body ast)) )
                  (cons (:this ast))
                  (map transform-ast))})

(defmethod transform-ast :new [ast]
  {:op :new :children (->> (cons (:class ast) (vec-to-list (:args ast)))
                           (map transform-ast))})

(defmethod transform-ast :primitive-invoke [ast]
  {:op :primitive-invoke
   :children (->> (cons (:fn ast) (vec-to-list (:args ast)))
                  (map transform-ast))})

(defmethod transform-ast :protocol-invoke [ast]
  {:op :protocol-invoke
   :children (->> (cons (:protocol-fn ast)
                        (cons (:target ast) (vec-to-list (:args ast))))
                  (map transform-ast))})

(defmethod transform-ast :recur [ast]
  {:op :recur :children (map transform-ast (:exprs ast))})

(defmethod transform-ast :reify [ast]
  {:op :reify :children (map transform-ast (:methods ast))})

(defmethod transform-ast :set [ast]
  {:op :set :children (map transform-ast (:items ast))})

(defmethod transform-ast :static-call [ast]
  {:op :static-call :val (str (.getName (:class ast)) "/" (:method ast)) 
   :children (map transform-ast (:args ast))})

(defmethod transform-ast :static-field [ast]
  {:op :static-field
   :val (str (.getName (:class ast)) "/" (:method ast))})

(defmethod transform-ast :the-var [ast]
  {:op :the-var :val (second (:form ast))})

(defmethod transform-ast :try [ast]
  {:op :try :children (->> (conj-not-nil (:catches ast) (:finally ast)) 
                           (cons (:body ast))
                           (map transform-ast))})

;; NOTE maybe use (second (:form ast)) instead of :var
(defmethod transform-ast :var [ast]
  {:op :var :val (:form ast)})

(defmethod transform-ast :vector [ast]
  {:op :vector :children (map transform-ast (:items ast))})

;; NOTE maybe remove this meta field
(defmethod transform-ast :with-meta [ast]
  {:op :with-meta :children (list (transform-ast (:expr ast)))})

(defn- bindings-body-case [ast]
  {:op (:op ast)
   :children (->> (conj (:bindings ast) (:body ast))
                  (map transform-ast))})

(defmethod transform-ast :let [ast] (bindings-body-case ast))
(defmethod transform-ast :letfn [ast] (bindings-body-case ast))
(defmethod transform-ast :loop [ast] (bindings-body-case ast))

(defn- children-case [ast]
  {:op (:op ast)
   :children (map transform-ast (map #(% ast) (:children ast)))})

(defmethod transform-ast :case-test [ast] (children-case ast))
(defmethod transform-ast :case-then [ast] (children-case ast))
(defmethod transform-ast :catch [ast] (children-case ast))
(defmethod transform-ast :host-interop [ast] (children-case ast))
(defmethod transform-ast :if [ast] (children-case ast))
(defmethod transform-ast :monitor-enter [ast] (children-case ast))
(defmethod transform-ast :monitor-exit [ast] (children-case ast))
(defmethod transform-ast :quote [ast] (children-case ast))
(defmethod transform-ast :set! [ast] (children-case ast))
(defmethod transform-ast :throw [ast] (children-case ast))

(defmethod transform-ast :default [ast]
  (println ast)
  (throw (Exception. "Undefined AST node !!!")))

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

(defn create-ast-paths-helper
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
    (let [children-results (map create-ast-paths-helper (:children ast))
          children-path-extensions (map first children-results)
          children-results (mapcat second children-results)
          {:keys [op val]} ast]
      [(cons {:path (list op) :val val}
             (extend-path-extensions op (apply concat children-path-extensions)))
       (concat (combine-path-extensions-direct op val children-path-extensions)
               (combine-path-extensions op children-path-extensions)
               children-results)])
    (contains? ast :children)
    (let* [children-results (map create-ast-paths-helper (:children ast))
           children-path-extensions (map first children-results)
           children-results (mapcat second children-results)]
      [(extend-path-extensions (:op ast) (apply concat children-path-extensions ))
       (concat (combine-path-extensions (:op ast) children-path-extensions)
               children-results)])
    ;; TODO check if node exists that only contains 
    :else  [(list {:path (list (:op ast)) :val (:val ast)}) '()]))

(defn create-ast-path
  "Creates an AST-PATH of the form [terminal-value1 path terminal-value2].

  The path is the form (:op1 \"<\" :op2 ... \">\" :op_n) where \"<\"
  designates an up operation and \"<\" a down operation. The input is a path
  as returned by the create-ast-paths-helper."
  [path]
  [(:val1 path)
   (concat (interleave (:path1 path) (repeat "<"))
           (list (:op path))
           (interleave (repeat ">") (:path2 path)))
   (:val2 path)])

(defn filter-for-defs [asts]
  (filter #(= (:op %) :def) asts))

(defn filter-for-with-vals [asts]
  (filter #(contains? % :val) asts))

(defn to-asts
  "Transforms a list of ASTs as returned by tools.analyzer by transform-ast."
  [asts]
  (map transform-ast asts))

(defn file-to-asts 
  "Returns a list of asts, one ast per expression in the file."
  [file]
  (->> (read-string-as-clj-exprs (slurp file))
       (map ana/analyze)
       to-asts))

(defn clojar-name-to-asts
  "Returns a list of asts, one ast per expression in the clojar namespaces."
  [name]
  (-> (retrieve/analyze-clojar-by-name name)
      to-asts))

(defn all-clojars-to-asts
  "Returns all the asts in the clojar repositories."
  []
  (->> (retrieve/analyze-clojar-non-forks)
       to-asts))

(defn string-to-asts [string]
  (->> (read-string-as-clj-exprs string)
       (map ana/analyze)
       (map transform-ast)))

(defn asts-to-ast-paths 
  "Creates all list of ast-path lists, one ast-path list per ast."
  [asts]
  (->> asts 
       (map create-ast-paths-helper)
       (map second)
       (map #(map create-ast-path %))))

(defn file-to-ast-paths 
  "Creates all list of ast-path lists, one ast-path list per expression."
  [file]
  (->> (file-to-asts file)
       (asts-to-ast-paths)))

(defn clojar-name-to-ast-paths 
  "Creates a list of ast-path lists from a clojar repository."
  [name]
  (->> (clojar-name-to-asts name)
       (asts-to-ast-paths)))

(defn all-clojars-to-ast-paths
  "Creates a list of ast-path lists from all clojar repositories."
  []
  (->> (all-clojars-to-asts)
       (asts-to-ast-paths)))

(defn code2vec-name-encoding 
  "Function name encoding as used by code2vec format."
  [name]
  ((comp
    #(if (= (first %) \|) (subs % 1) %)
    #(if (= (last %) \|) (subs % 0 (- (count %) 1)) %)
    #(st/replace % #"-" "|" ))
   name))

(defn hash-path 
  "Hashes an ast-path, outputs triple as used by code2vec."
  [ast-path]
  [(first ast-path)
   (->> (second ast-path) (map name) (apply str) hash)
   (nth ast-path 2)])

(defn asts-to-code2vec 
  "Transforms a list of asts to the code2vec format."
  [asts]
  (let [asts (filter-for-with-vals asts)
        vals (map #(->> % :val str code2vec-name-encoding) asts)
        ast-paths (->> asts
                       (map create-ast-paths-helper)
                       (map second)
                       (map #(map (comp hash-path create-ast-path) %)))]
    (map cons vals ast-paths)))

(defn file-to-code2vec 
  "Transforms a file to the code2vec format."
  [file]
  (asts-to-code2vec (file-to-asts file)))

(defn clojar-name-to-code2vec
  "Transforms a clojar project to the code2vec format."
  [name]
  (asts-to-code2vec (clojar-name-to-asts name)))

(defn all-clojars-to-code2vec
  "Transforms all clojar projects to the code2vec format."
  []
  (->> (retrieve/analyze-clojar-non-forks)
       to-asts
       asts-to-code2vec))

(comment
 (set! *print-length* 10)
 (set! *print-level* 10)

 (require '[clojure.tools.reader :as r])
 (require '[clojure.tools.reader.reader-types :as t])
 (binding [r/*read-eval* false]
   (loop [input (t/source-logging-push-back-reader "(+ #=(+ 1 2) 3) :foo")
          res '[]]
     (let [exp (try (r/read input false :end)
                    (catch clojure.lang.ExceptionInfo e :read-err))]
       (if (= exp :end) res
           (recur input (if (= exp :read-err) res (conj res exp)))))))
 ;; => [(+ 1 2) 3 :foo]
 ;; => want just [:foo]
 )
