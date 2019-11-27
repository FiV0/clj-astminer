(ns clj-astminer.retrieve
  (:require [clojure.tools.deps.alpha :as deps]
            [clojure.tools.deps.alpha.extensions :as ext])
  (:gen-class))

(comment
  ;; authentication issue, don't know why
  (deps/resolve-deps
   '{:deps
     {next.jdbc {:git/url "https://github.com/seancorfield/next-jdb.git"
                 :sha "32e9f338d5ad09c1f47aaa960e01931cfc70d02d"}}}
   nil)
  ;; lein currently does not work
  (deps/resolve-deps
   '{:deps
     {compojure.core {:git/url "https://github.com/weavejester/compojure.git"
                      :sha "bb2fcc7ffdc910555d24ff64a8e7ffacb2c1c478"}}}
   nil))
