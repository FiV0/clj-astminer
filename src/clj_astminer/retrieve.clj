(ns clj-astminer.retrieve
  (:require ;; [cemerick.pomegranate :as pome]
   [clj-http.client :as client]
   [clojure.java.io :as io] 
   [clojure.tools.analyzer.jvm :as ana]
   [clojure.tools.deps.alpha.util.maven :as mvn]
   [clojure.tools.deps.alpha.repl :refer [add-lib]]
   ;; [clojure.tools.deps.alpha :as deps] ;; curently not in a good state
   ;; [clojure.tools.deps.alpha.extensions :as ext]
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
   (map clojure.edn/read-string v)
   (map #(assoc % :version (first (:versions %))) v)))

(defn get-clojars-non-forks []
  (->> (get-clojars-pom-mappings)
       (filter (fn [m] (= (:artifact-id m) (:group-id m))))))

(defn get-clojar-by-name [name]
  (-> (client/get (str "https://clojars.org/api/artifacts/" name)
                  {:accept :json :as :json})
      :body
      (clojure.set/rename-keys {:group_name :group-id
                                :jar_name :artifact-id
                                :latest_version :version})))

(def repo-root "~/.m2/repository")

(defn create-jar-path-from-clojar-map
  "Creates the default path where the newest version of an artifact is stored on disk."
  [m]
  (->>
   (list repo-root
         (clojure.string/replace (:group-id m) "." "/")
         (:artifact-id m)
         (:version m)
         (str (:artifact-id m) "-" (:version m) ".jar"))
   (interleave (repeat "/"))
   rest
   (apply str)))


(defn add-dependency-from-clojar-map
  "Adds a dependency from a clojar mapping. Uses newest version."
  [m]
  (try
    (do
      (binding [*warn-on-reflection* false]
        (add-lib `~(symbol (:artifact-id m)) {:mvn/version (:version m)}))
      true)
    (catch Exception e
      (do (println "Failed getting dependency " (:artifact-id m))
          (println "Reason: "(ex-data e))
          false)))
  ;; (pome/add-dependencies
  ;;  :coordinates `[[~(symbol (:artifact-id m))
  ;;                  ~(:version m)]]
  ;;  :repositories (merge cemerick.pomegranate.aether/maven-central
  ;;                       {"clojars" "https://clojars.org/repo"})
  ;; :classloader (.getParent @Compiler/LOADER)
  ;; )
  )

(defn add-dependencies-from-clojar-maps
  "Adds dependencies from clojar mappings. Uses newest version."
  [ms]
  (every? true? (for [m ms] (add-dependency-from-clojar-map m)))
  ;; (pome/add-dependencies
  ;;  :coordinates `[~@(map #(vector (symbol (:artifact-id %))
  ;;                                 (:version %))
  ;;                        ms)]
  ;;  :repositories (merge cemerick.pomegranate.aether/maven-central
  ;;                       {"clojars" "https://clojars.org/repo"})
  ;;  ;; :classloader (.getParent @Compiler/LOADER)
  ;;  )
  )

(defn namespaces-in-jar
  "Enumerates the namespaces in th"
  [jar-file-path]
  (-> jar-file-path
      expand-home
      java.util.jar.JarFile.
      (ns-find/find-namespaces-in-jarfile ns-find/clj)))

(defn require-from-clojar-map
  "Adds dependency from clojar mapping."
  [m]
  (if (add-dependency-from-clojar-map m)
    (let [nss (-> m
                  create-jar-path-from-clojar-map
                  namespaces-in-jar)
          res (for [ns nss]
                (try (do
                       (require `~ns)
                       ns)
                     (catch clojure.lang.Compiler$CompilerException e
                       (prn "Compiler exception: " (ex-data e)))
                     (catch Error e
                       (prn "Requiring Error: " e))
                     ;; (catch clojure.lang.ExceptionInfo e
                     ;;   (prn "Exception information: " (ex-data e)))
                     ;; (catch Exception e
                     ;;   (prn "General exception:" (ex-data e)))
                     ))]
      (if (some nil? res)
        '()
        res)
      '())))

(defn remove-nss 
  "Remove namespaces."
  [nss]
  (map remove-ns nss))

(defn analyze-ns-error-prone [ns]
  "Analyzes a namespace and returns nil in case of an error."
  (try (ana/analyze-ns ns)
       (catch Exception e
         (prn "General exception:" (ex-data e)))))

(defn analyze-from-clojar-map [m]
  (prn "Analyzing " (:artifact-id m))
  (let [nss (require-from-clojar-map m)
        res (->> (map analyze-ns-error-prone nss)
                 (filter #(not (nil? %))))]
    (remove-nss nss)
    (prn "Analyzed " (:artifact-id m))
    res))

(defn analyze-clojar-by-name [name]
  (->> (get-clojar-by-name name)
       analyze-from-clojar-map
       (apply concat)))

(defn analyze-clojar-non-forks
  ([] (analyze-clojar-non-forks -1))
  ([limit]
   (as-> (get-clojars-non-forks) v
     (take (if (= -1 limit) (count v) limit) v)
     (map analyze-from-clojar-map v)
     (map #(apply concat %) v)
     (apply concat v))))

(comment
  (set! *print-length* 10)
  (set! *print-level* 10)
  (count (analyze-clojar-non-forks 10))
  )

;; currently not used
(comment
 (defn entries [jarfile]
   (enumeration-seq (.entries jarfile)))

 (defn walkjar [fileName]
   (with-open [z (java.util.jar.JarFile. fileName)]
     (doseq [e (entries z)]
       (println (.getName e)))))

 (defn jar-file? [file]
   (clojure.string/ends-with? file ".jar")))

;; curently tools.deps.alpha not in beta
(comment
  (require '[clojure.tools.deps.alpha.util.maven :as mvn])
  (require '[clojure.tools.deps.alpha.repl :refer [add-lib]])
  (require '[clojure.tools.gitlibs :as gl])
  (defn load-master [lib]
    (let [git (str "https://github.com/" lib ".git")]
      (add-lib lib {:git/url git :sha (gl/resolve git "master")})))
  (add-lib 'org.clojure/tools.trace {:mvn/version "0.7.10" }) ;; works
  (load-master 'clojure/tools.trace) ;; doesnt
  (add-lib 'org.clojure/data.json {:mvn/version "0.2.7" }) ;; works
  (load-master 'clojure/data.json) ;; works

  ;; lein currently does not work
  ;; (load-master 'weavejester/compojure)
  (deps/resolve-deps
   '{:deps
     {compojure.core {:git/url "https://github.com/weavejester/compojure.git"
                      :sha "bb2fcc7ffdc910555d24ff64a8e7ffacb2c1c478"}}}
   nil)
  )
