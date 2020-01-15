(ns clj-astminer.gitlisting
  (:require [clojure.tools.deps.alpha.util.maven :as mvn]
            [clojure.tools.deps.alpha.repl :refer [add-lib]]
            [clojure.tools.gitlibs :as gl]
            [tentacles.search :as search]
            [tentacles.core :as core]
            [tentacles.repos :as repos])
  (:gen-class))

(defn get-clojure-repos []
  (->> (search/search-repos "" {:language "clojure"} {:sort "stars" :order "desc"})
       :items))

(defn get-latest-release [repo]
  (let [name (:name repo)
        owner (->> repo :owner :login)
        release-tag (->> (repos/tags owner name) first :name)]
    {:name name :owner owner :tag release-tag}))

(defn load-master [lib]
  (let [git (str "https://github.com/" lib ".git")]
    (add-lib lib {:git/url git :sha (gl/resolve git "master")})))

(defn load-lib-from-tag [release]
  (let [lib (symbol (str (:owner release) "/" (:name release)))
        git (str "https://github.com/" lib ".git")]
    (add-lib lib {:git/url git :sha (gl/resolve git (:tag release))})))

(comment
  (->> (get-clojure-repos)
       first
       get-latest-release
       load-lib-from-tag))
