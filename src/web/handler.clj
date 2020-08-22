(ns web.handler
  (:use capacity.core
        ring.middleware.resource
        ring.middleware.nested-params
        ring.middleware.params
        ring.util.response
        ring.adapter.jetty)
  (:require [capacity.config :as config]
            [capacity.report :as report]
            [capacity.utils :as utils]
            [web.router :as router]
            [web.templates :as t]
            [clojure.string :as s]
            [clojure.set :as st]
            [hiccup.core :as h])
  (:import [capacity.core Eng Project]))

(defn available-profs
  [config]
  (apply st/union
         (-> config :profs vals)))

(defn summarize-config [filename]
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
             [["input" "Sprints" "constants[sprints]" sprints]
              ["input" "Unplanned" "constants[unplanned]" unplanned]
              ["input" "Velocity" "constants[velocity]" velocity]])))

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
  [iteration contrib-iter]
  (map #(let [[eng contrib] %
              eng-name (name eng)]
          (t/input "input"
                   eng-name
                   (format "contrib[%d][%s]"
                           iteration
                           eng-name)
                   contrib))
       contrib-iter))

(defn render-contrib-iter
  [iteration contrib-iter]
  (into [:fieldset
         [:legend (str "Iteration " iteration)]]
        (render-contrib-iter-inputs iteration contrib-iter)))

(defn render-contribs
  [& contribs]
  (into [:fieldset
         [:legend "Contributions"]]
        (map (partial apply render-contrib-iter)
             (utils/group-interleave (range) contribs))))

(defn render-prof-val
  [project-name prof value]
  (t/input "input"
           (format "%s effort" prof)
           (format "project[%s][effort][%s]"
                   project-name
                   prof)
           value))

(defn render-effort
  [project-name effort all-profs]
  (map #(render-prof-val project-name
                         (name %)
                         (or (effort %) 0))
       all-profs))

(defn render-project
  [number project all-profs]
  (let [{name :name} project]
    [:fieldset name
     (t/input "input"
              "Rank"
              (format "project[%s][rank]" name)
              number)
     (t/input "input"
              "Name"
              (format "project[%s][name]" name)
              name)
     (render-effort name (:effort project) all-profs)]))

(defn with-response [resp]
  (-> (response (h/html (t/template resp)))
      (content-type "text/html")
      (status 200)))

(defn routes [request]
  (let [{uri    :uri
         params :params} request]
    (router/routes
     uri
     "/{config-name}"
     (fn [{config-name :config-name}]
       (with-response
         (map (comp t/section render-summary)
              (summarize-config (str config-name ".edn")))))

     "/input/{config-name}"
     (fn [{config-name :config-name}]
       (let [config (config/read (str config-name ".edn"))]
         (with-response
           [:form {:action (format "/input/%s/submit" config-name)
                   :method "POST"}
            [:button "Submit"]
            (apply render-constants-inputs
                   (map (:constants config) [:sprints :unplanned :velocity]))
            (render-profs-inputs (:profs config))
            (apply render-contribs (:contrib config))
            (into [:fieldset "Projects"]
                  (map #(render-project (first %)
                                        (last %)
                                        (available-profs config))
                       (utils/group-interleave (range)
                                               (:projects config))))])))

     "/input/{config-name}/submit" ;; change this to be a POST
     (fn [{config-name :config-name}]
       (with-response [:div
                       [:p (str "Saved config " config-name)]
                       [:p (str (-> request
                                    nested-params-request
                                    :params))]]))
     (-> (response "Page not found")
         (status 404)))))

(def app (wrap-params routes))

