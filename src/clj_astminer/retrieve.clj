(ns clj-astminer.retrieve
  (:require [clj-http.client :as client] 
            [clojure.tools.deps.alpha :as deps] ;; curently not in a good state
            [clojure.tools.deps.alpha.extensions :as ext])
  (:gen-class))

(defn get-clojars-lein-artifacts []
  (->
   (client/get "http://clojars.org/repo/all-jars.clj")
   :body
   (clojure.string/split #"\n")))

(defn get-clojars-pom-mappings []
  (as->
   (client/get "https://clojars.org/repo/feed.clj.gz" {:as :byte-array}) v
   (:body v)
   (java.io.ByteArrayInputStream. v)  
   (java.util.zip.GZIPInputStream. v)
   (slurp v)
   (clojure.string/split v #"\n")
   (map clojure.edn/read-string v)))

(comment
 (get-clojars-pom-mappings)
 (def res *1)
 (->> (filter (fn [m] (-> m :artifact-id (clojure.string/includes? "pomegranate"))) res)
      (map :artifact-id)
      (map println)
      dorun)

 (-> res first keys)
 (-> res first :artifact-id)

 (def artefact-url (str "https://clojars.org/api/artifacts/" *1))
 (client/get artefact-url)
 artefact-url
 )

(comment
 (set! *print-length* 10)
 (set! *print-level* 10)
 )

;; curently tools.deps.alpha not in beta
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
