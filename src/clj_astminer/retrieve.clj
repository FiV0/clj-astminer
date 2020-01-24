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

;; to not remove the projects own dependencies and
;; projects that I had issues with
(def forbidden-projects
  ["cheshire" "clj-http" "clj-jgit" "clojure" "core.match" "math.combinatorics"
   "tools.gitlibs" "tools.analyzer.jvm" "tools.namespace" "tools.trace"
   "tools.cli" "tentacles" "tools.deps.alpha" "speculative"])

(defn in? 
  "True if coll contains elm."
  [coll elm]  
  (some #(= elm %) coll))

(defn get-clojars-non-forks []
  (->> (get-clojars-pom-mappings)
       (filter (fn [m] (= (:artifact-id m) (:group-id m))))
       (filter (fn [m] (not (in? forbidden-projects (:artifact-id m)))))))

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
      (binding [*warn-on-reflection* false] ;; TODO doesn't weem to work
        (add-lib `~(symbol (:artifact-id m)) {:mvn/version (:version m)}))
      true)
    (catch Exception e
      (do (println "Failed getting dependency " (:artifact-id m))
          (println "Reason: "(ex-data e))
          false))))

(defn add-dependencies-from-clojar-maps
  "Adds dependencies from clojar mappings. Uses newest version."
  [ms]
  (every? true? (for [m ms] (add-dependency-from-clojar-map m))))

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
                       (prn "Compiler exception: " e))
                     (catch java.io.FileNotFoundException e
                       (prn "FileNotFound Error: " e))
                     (catch Error e
                       (prn "General error:" e))
                     (catch Exception e
                       (prn "General exception:" e))
                     ))]
      (if (some nil? res) 
        '()
        res))
    '()))

(defn remove-nss 
  "Remove namespaces."
  [nss]
  (map remove-ns nss))

(defn analyze-ns-error-prone [ns]
  "Analyzes a namespace and returns nil in case of an error."
  (try (ana/analyze-ns ns)
       (catch Exception e
         (prn "General exception:" (ex-data e)))
       (catch Error e
         (prn "General error:" e))))

(defn analyze-from-clojar-map
  ([m] (analyze-from-clojar-map -1 m))
  ([index m]
   (prn "Analyzing " (:artifact-id m) index)
   ;; #dbg ^{:break/when (= (:artifact-id m) "edos")}
   (let [nss (require-from-clojar-map m)
         res (->> (map analyze-ns-error-prone nss)
                  (filter #(not (nil? %))))]
     (remove-nss nss)
     (prn "Analyzed " (:artifact-id m))
     res)))

(defn analyze-clojar-by-name [name]
  (->> (get-clojar-by-name name)
       analyze-from-clojar-map
       (apply concat)))


(defn analyze-clojar-non-forks
  ([] (analyze-clojar-non-forks -1))
  ([limit]
   (as-> (get-clojars-non-forks) v
     (take (if (= -1 limit) (count v) limit) v)
     ;; (reverse v)
     (map-indexed analyze-from-clojar-map v)
     (map #(apply concat %) v)
     (apply concat v))))

(comment
  (set! *print-length* 10)
  (set! *print-level* 10)
  (->>
   (take 2 (get-clojars-non-forks))
   (map analyze-from-clojar-map))
  (require '[edos.core])
  (analyze-ns-error-prone 'edos.core))

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

  (->> (take 100 (get-clojars-non-forks))
       (map :group-id)
       (map-indexed println))
  (->> (get-clojars-non-forks)
       (map :group-id)
       (map-indexed #(vector %1 %2))
       (drop-while #(not= (second %) "edos")))

  )


(comment
  (->> (get-clojar-by-name "duct.logger.honeybadger")
       create-jar-path-from-clojar-map
       namespaces-in-jar
       ;; (require-from-clojar-map)
       )
  (def nss *1)
  nss
  (for [ns nss] (println ns))

  (add-lib 'duct.logger.honeybadger {:mvn/version "0.2.0"})
  (add-lib 'org.clojure/tools.namespace {:mvn/version "0.3.1"})
  (add-lib 'org.clojure/java.classpath {:mvn/version "0.3.0"})
  (clojure.tools.namespace.find/find-namespaces)
  (->>
   (for [jar (clojure.java.classpath/classpath-jarfiles)]
     (.getName ^java.util.jar.JarFile jar))
   (filter #(clojure.string/includes? % "duct")))
  (require '[duct.logger.honeybadger])

  (def res (get-clojars-non-forks)))
