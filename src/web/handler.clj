(ns web.handler
  (:use capacity.core
        ring.middleware.resource
        ring.util.response
        ring.adapter.jetty)
  (:require [capacity.config :as config]
            [capacity.report :as report]
            [capacity.utils :as utils])
  (:import [capacity.core Eng Project]))

(defn summarize [filename]
  (let [[backlog teams] (config/to-models filename)
        res (rest (work-backlog-iter backlog teams))
        res-w-teams (utils/insert teams 2 res)]
    (map (partial apply report/full-summary)
         (apply utils/group-interleave res-w-teams))))

(defn with-response [resp]
  (-> (response resp)
      (content-type "text/html")
      (status 200)))

(defn routes [{uri :uri}]
  (case uri
    "/" (with-response (summarize "config-fy21.edn"))
    (-> (response "Page not found")
        (status 404))))

(def app routes)

