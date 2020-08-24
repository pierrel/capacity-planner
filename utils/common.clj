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

(defn build-cljs [opts]
  (cljsc/build "src"
               opts))

(defn prod-build-cljs []
  (build-cljs {:main          'web.js.core
               :output-to     "resources/public/js/main.js"
               :optimizations :advanced}))

(defn dev-build-cljs []
  (fs/delete-dir top-resources)
  (build-cljs {:output-to  (str resources-dir "/" js-file)
               :output-dir resources-dir}))

(defn app [dev?]
  (if dev?
    (-> #'handler/app wrap-file wrap-reload)
    #'handler/app))

(defn run-server
  "Runs and returns the server"
  [port block? dev?]
  (run-jetty (app dev?) {:port port :join? block?}))
