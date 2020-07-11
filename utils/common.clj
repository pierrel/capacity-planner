(ns common
  (:require
   [web.handler :as handler]
   [ring.adapter.jetty :refer [run-jetty]]
   [ring.middleware.reload :refer [wrap-reload]]
   [ring.middleware.file :refer [wrap-file]]))

(defn app [dev?]
  (if dev?
    (wrap-reload #'handler/app)
    #'handler/app))

(defn run-server
  "Runs and returns the server"
  [port block? dev?]
  (run-jetty (app dev?) {:port port :join? block?}))
