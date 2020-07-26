(ns web.handler
  (:use capacity.core
        ring.middleware.resource
        ring.util.response
        ring.adapter.jetty)
  (:require [capacity.config :as config]
            [capacity.report :as report]
            [capacity.utils :as utils]
            [web.router :as router]
            [hiccup.core :as h])
  (:import [capacity.core Eng Project]))

(defn summarize [filename]
  (let [[backlog teams] (config/to-models filename)
        res (rest (work-backlog-iter backlog teams))
        res-w-teams (utils/insert teams 2 res)]
    (map (partial apply report/full-summary)
         (apply utils/group-interleave res-w-teams))))

(defn render-summary-item
  [heading lst]
  [:div
   [:h2 heading]
   [:ul
    (map #(vector :li %) lst)]])

(defn render-summary [summary]
  (map #(render-summary-item (-> % first name) (last %)) summary))

(defn section
  [form]
  [:div form])

(defn with-response [resp]
  (-> (response (h/html resp))
      (content-type "text/html")
      (status 200)))

(defn routes [{uri :uri}]
  (router/routes uri
                 "/" (with-response (map (comp section render-summary)
                                         (summarize "config-fy21.edn")))
                 (-> (response "Page not found")
                     (status 404))))

(def app routes)

