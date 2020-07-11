(ns server
  (:require [common :as common])
  (:gen-class))

(def port
  (let [env-port (System/getenv "PORT")]
    (if env-port (Integer/parseInt env-port) 3000)))

(defn -main
  "Run the server"
  [& args]
  (common/run-server port
                     true
                     true))
