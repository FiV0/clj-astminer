(ns clj-astminer.retrieve
  (:require [cemerick.pomegranate :as pome]
            [clj-http.client :as client] 
            [clojure.tools.deps.alpha :as deps] ;; curently not in a good state
            [clojure.tools.deps.alpha.extensions :as ext]
            [clojure.tools.namespace.find :as ns-find])
  (:gen-class))

(defn expand-home [s]
  (if (.startsWith s "~")
    (clojure.string/replace-first s "~" (System/getProperty "user.home"))
    s))

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

(defn get-clojars-non-forks []
  (->> (get-clojars-pom-mappings)
       (filter (fn [m] (= (:artifact-id m) (:group-id m))))))

(defn namespaces-in-jar [jar-file-path]
  "Enumerates the namespaces in th"
  (-> jar-file-path
      expand-home
      java.util.jar.JarFile.
      ns-find/find-namespaces-in-jarfile))

;; (namespaces-in-jar "~/.m2/repository/clj-http/clj-http/3.10.0/clj-http-3.10.0.jar")

(defn entries [jarfile]
  (enumeration-seq (.entries zipfile)))

(defn walkjar [fileName]
  (with-open [z (java.util.jar.JarFile. fileName)]
             (doseq [e (entries z)]
               (println (.getName e)))))

(comment
 (pome/add-dependencies :coordinates '[[incanter "1.9.2"]]
                        :repositories (merge cemerick.pomegranate.aether/maven-central
                                             {"clojars" "https://clojars.org/repo"})))

(comment
 (set! *print-length* 10)
 (set! *print-level* 10)
 )

(comment
 (get-clojars-pom-mappings)
 (def res *1)
 (-> res first keys)
 (-> res first :artifact-id)

 (def artefact-url (str "https://clojars.org/api/artifacts/" *1))
 (client/get artefact-url)
 artefact-url
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
