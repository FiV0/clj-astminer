(ns clj-astminer.astminer-test
  (:require [clj-astminer.astminer :refer :all]
            [clojure.test :refer :all]
            [clojure.tools.reader :as r]))

(deftest read-string-as-clj-exprs-test
  (binding [r/*read-eval* false]
   (testing "with no reader macros"
     (is (= 2 (count (read-string-as-clj-exprs (slurp "resources/test.clj"))))))
   (testing "with reader macros"
     (is (= 2 (count (read-string-as-clj-exprs "#=(+ 1 2)\n (defn id [x] x)")))))))

(deftest file-to-asts-test  
  (binding [r/*read-eval* false]
    (testing "with resources/test.clj"
      (is (= 2 (count (file-to-asts "resources/test.clj")))))))

(deftest reader-conditional-test  
  (binding [r/*read-eval* false]
    (testing "with resources/reader-conditional-test.clj"
      (is (= 1 (count (file-to-asts "resources/reader-conditional-test.clj")))))))

(deftest extend-path-extensions-test
  (testing "extend-path-extensions"
    (is (= '({:path (:foo :bar)} {:path (:foo :buzz)})
           (extend-path-extensions :foo '({:path (:bar)} {:path (:buzz)}))))))

(deftest combine-path-extensions-test
  (testing "combine-path-extensions"
    (is (= '({:path1 (:bar) :val1 1 :op :foo :path2 (:buzz) :val2 2})
           (combine-path-extensions :foo '(({:path (:bar) :val 1})
                                           ({:path (:buzz) :val 2})))))))

(deftest combine-path-extensions-direct-test
  (testing "combine-path-extensions"
    (is (= '({:path1 nil :val1 "foo" :op :foo :path2 (:bar) :val2 1}
             {:path1 nil :val1 "foo" :op :foo :path2 (:buzz) :val2 2})
           (combine-path-extensions-direct :foo "foo" '(({:path (:bar) :val 1})
                                                        ({:path (:buzz) :val 2})))))))

(deftest file-to-ast-paths-test  
  (binding [r/*read-eval* false]
    (testing "with resources/test.clj"
      (is (= 2 (count (file-to-ast-paths "resources/test.clj")))))))

(deftest file-to-code2vec-test
  (binding [r/*read-eval* false]
    (testing "with resources/test.clj"
      (is (= 1 (count (file-to-code2vec "resources/test.clj")))))))

;; (ns-unmap 'clj-astminer.astminer-test '-main)
;; (run-tests)
;; (reader-conditional-test)
