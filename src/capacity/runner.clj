(ns capacity.runner
  (:require [capacity.config :as config])
  (:use [capacity.core]
        [clojure.pprint]))

(defn report-on [result]
  (println "Completed: " (:completed result))
  (println "Made progress on: " (map :name (:progressed result)))
  (println "Left with capacity: " (:remaining-capacity result)))

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

(defn -main [& args]
  (run-and-report  (or (first args)
                       "config.edn")))

