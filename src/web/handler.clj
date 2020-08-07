(ns web.handler
  (:use capacity.core
        ring.middleware.resource
        ring.util.response
        ring.adapter.jetty)
  (:require [capacity.config :as config]
            [capacity.report :as report]
            [capacity.utils :as utils]
            [web.router :as router]
            [web.templates :as t]
            [clojure.string :as s]
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

(defn render-constants-inputs
  [sprints unplanned velocity]
  (into [:fieldset
         [:legend "Constants"]]
        (map (partial apply t/input)
             [["input" "Sprints" sprints]
              ["input" "Unplanned" unplanned]
              ["input" "Velocity" velocity]])))

(defn render-profs-inputs
  [eng-profs]
  (into [:fieldset
         [:legend "Proficiencies"]]
        (map #(let [[eng profs] %]
                (t/input "input"
                         (name eng)
                         (s/join ", " (map name profs))))
             eng-profs)))

(defn render-contrib-iter-inputs
  [contrib-iter]
  (map #(let [[eng contrib] %]
          (t/input "input"
                   (name eng)
                   contrib))
       contrib-iter))

(defn render-contrib-iter
  [iteration contrib-iter]
  (into [:fieldset
         [:legend (str "Iteration " iteration)]]
        (render-contrib-iter-inputs contrib-iter)))


(defn render-contribs
  [& contribs]
  (into [:fieldset
         [:legend "Contributions"]]
        (map (partial apply render-contrib-iter)
             (utils/group-interleave (range) contribs))))

(defn with-response [resp]
  (-> (response (h/html (t/template resp)))
      (content-type "text/html")
      (status 200)))

(defn routes [{uri :uri}]
  (router/routes
   uri
   "/{config-name}"
   (fn [{config-name :config-name}]
     (with-response
       (map (comp t/section render-summary)
            (summarize (str config-name ".edn")))))

   "/input/{config-name}"
   (fn [{config-name :config-name}]
     (with-response
       (let [config (config/read (str config-name ".edn"))]
         [(apply render-constants-inputs
                 (map (:constants config) [:sprints :unplanned :velocity]))
          (render-profs-inputs (:profs config))
          (apply render-contribs (:contrib config))])))
   (-> (response "Page not found")
       (status 404))))

(def app routes)

