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
