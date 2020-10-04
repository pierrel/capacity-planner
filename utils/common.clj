(ns common
  (:require
   [web.handler :as handler]
   [cljs.closure :as cljsc]
   [me.raynes.fs :as fs]
   [ring.adapter.jetty :refer [run-jetty]]
   [ring.middleware.reload :refer [wrap-reload]]
   [ring.middleware.file :refer [wrap-file]]))

(def top-resources "dev_resources")
(def resources-dir (str top-resources "/public"))
(def js-file "repl.js")

(defn refresh-resources
  "Destroys and creates the appropriate resources."
  []
  (println "Deleting resources")
  (fs/delete-dir top-resources)
  (println "Re-creating resources")
  (fs/mkdirs resources-dir))

(defn build-cljs [opts]
  (cljsc/build "src"
               opts))

(defn dev-build-cljs []
  (println "Generating dev js")
  (build-cljs {:output-to  (str resources-dir "/" js-file)
               :output-dir resources-dir}))

(defn app [dev?]
  (refresh-resources)
  (if dev?
    (wrap-reload (wrap-file #'handler/app resources-dir))
    #'handler/app))

(defn run-server
  "Runs and returns the server"
  [port block? dev?]
  (run-jetty (app dev?) {:port port :join? block?}))
