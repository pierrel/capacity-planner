(ns capacity.runner
  (:require [capacity.config :as config]
            [capacity.modeling :as models]
            [capacity.utils :as utils])
  (:use [capacity.core]
        [clojure.pprint])
  (:import [capacity.modeling Eng Project]))

(defn report-on [result]
  (println "Completed: " (:completed result))
  (println "Made progress on: " (map :name (:progressed result)))
  (println "Left with capacity: " (:remaining-team result)))

(defn run-and-report [filename]
  (let [conf     (config/read filename)
        projects (:projects conf)
        const    (:constants conf)
        results  (work-on-long projects
                               (:contrib conf)
                               const
                               (:profs conf))]
    (doseq [[iter result] (partition 2 (interleave (rest (range)) results))]
      (println "Iteration " iter)
      (report-on result)
      (println))
    results))
;; Need to re-write these not to take "key"
(defn remaining
  [key original summary]
  (let [change (-> summary :diff key)
        orig-value (get original key)]
    (-> (+ change orig-value) zero? not)))

(defn progressed
  [key original summary]
  (let [nonzero? (complement zero?)]
    (and (some #(nonzero? (last %)) (get original key))
         (some #(nonzero? (last %)) (get summary :diff)))))

(defn completed
  [key original summary]
  (and (progressed key original summary)
       (every? zero? (map last (merge-with +
                                           (get original key)
                                           (:diff summary))))))

(defn summarize
  [filt original summary]
  (map #(-> % first :name)
       (filter #(apply filt %)
               (utils/group-interleave original summary))))

(defn run-models [filename]
  (let [[backlog iterations] (config/to-models filename)
        [rem-backlog
         backlogs
         backlog-summaries
         team-summaries] (models/work-backlog-iter backlog iterations)]
    (doseq [[iter
             iter-backlog
             backlog-summary
             iter-team
             team-summary] (utils/group-interleave (range)
                                                   backlogs
                                                   backlog-summaries
                                                   iterations
                                                   team-summaries)]
      (println "Iteration " iter)
      (println "Completed " (summarize (partial completed :effort)
                                       iter-backlog
                                       backlog-summary))
      (println "Made progress on " (summarize (partial progressed :effort)
                                              iter-backlog
                                              backlog-summary))
      (println "Left with capacity" (summarize (partial remaining :capacity)
                                               iter-team
                                               team-summary))
      (println))))

(defn -main [& args]
  (let [file (or (first args) "config.edn")]
    (run-and-report file)
    (run-models file)))
