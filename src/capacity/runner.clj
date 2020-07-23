(ns capacity.runner
  (:require [capacity.config :as config]
            [capacity.utils :as utils]
            [capacity.report :as report]
            [clojure.string :as s])
  (:use [capacity.core]
        [clojure.pprint])
  (:import [capacity.core Eng Project]))


(defn report-str
  [iteration rmap]
  (let [rmap-str (into {} (mapv #(vector (first %)
                                         (s/join ", " (last %)))
                                rmap))]
    (str "Iteration " iteration "\n"
         "Completed " (:completed rmap-str) "\n"
         "Made progress on " (:progressed rmap-str) "\n"
         "Left with capacity " (:leftover rmap-str) "\n")))

(defn report
  [iter iter-backlog backlog-summary iter-team team-summary]
  (report-str iter (report/full-summary iter-backlog
                                        backlog-summary
                                        iter-team
                                        team-summary)))

(defn run
  [filename]
  (let [[backlog iterations] (config/to-models filename)
        [remaining-backlog
         backlogs
         backlog-summaries
         team-summaries] (work-backlog-iter backlog iterations)]
    [remaining-backlog
     backlogs
     backlog-summaries
     iterations
     team-summaries]))

(defn run-and-report [filename]
  (let [[rem-backlog
         backlogs
         backlog-summaries
         iterations
         team-summaries] (run filename)]
    (s/join "\n" (map #(apply report %)
                      (utils/group-interleave (range)
                                              backlogs
                                              backlog-summaries
                                              iterations
                                              team-summaries)))))

(defn -main [& args]
  (println "running")
  (let [file (or (first args) "config.edn")]
    (println (run-and-report file))))
