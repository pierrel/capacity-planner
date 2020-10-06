(ns web.views.output
  (:require [web.templates :as t]
            [capacity.core :as core]
            [capacity.report :as report]
            [capacity.utils :as utils]
            [capacity.config :as config]))

(defn render-summary-item
  [heading lst]
  [:div
   [:h2 heading]
   [:ul
    (map (fn [[project change]]
           [:li (format "Project: %s, diff: %s"
                        (:name project)
                        (-> change :diff str))])
         lst)]])

(defn render-summary [summary]
  (map #(render-summary-item (-> % first name) (last %)) summary))

(defn summarize-config [filename-or-config]
  (let [[backlog teams] (config/to-models filename-or-config)
        res (rest (core/work-backlog-iter backlog teams))
        res-w-teams (utils/insert teams 2 res)]
    (map (partial apply report/full-summary-struct)
         (apply utils/group-interleave res-w-teams))))

(defn summary
  [conf]
  (map (comp t/section render-summary)
       (summarize-config conf)))
