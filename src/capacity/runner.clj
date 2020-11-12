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
  (let [all (concat (:completed rmap) (:progressed rmap))]
    (format "Iteration %d: %s"
            iteration
            (s/join ", " all))))

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
         team-summaries] (work-backlog-entirely backlog iterations)]
    [remaining-backlog
     backlogs
     backlog-summaries
     iterations
     team-summaries]))

(defn report-iterations
  [backlogs
   backlog-summaries
   iterations
   team-summaries]
  (format "Iterations:\n%s"
          (s/join "\n" (map #(apply report %)
                            (utils/group-interleave (map inc (range))
                                                    backlogs
                                                    backlog-summaries
                                                    iterations
                                                    team-summaries)))))

(defn report-backlog
  [backlog]
  (format "Backlog:\n%s"
          (s/join "\n" (map :name backlog))))

(defn run-and-report [filename]
  (let [[rem-backlog
         backlogs
         backlog-summaries
         iterations
         team-summaries] (run filename)]
    (format "%s\n\n%s"
            (report-backlog (first backlogs))
            (report-iterations backlogs
                               backlog-summaries
                               iterations
                               team-summaries))))

(defn -main [& args]
  (let [file (or (first args) "config.edn")]
    (println (run-and-report file))))
