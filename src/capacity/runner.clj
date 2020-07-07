(ns capacity.runner
  (:require [capacity.config :as config]
            [capacity.utils :as utils])
  (:use [capacity.core]
        [clojure.pprint])
  (:import [capacity.core Eng Project]))

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
  (every? zero? (map last (merge-with +
                                      (get original key)
                                      (:diff summary)))))

(defn summarize
  [filt original summary]
  (map #(-> % first :name)
       (filter #(apply filt %)
               (utils/group-interleave original summary))))

(defn run-and-report [filename]
  (let [[backlog iterations] (config/to-models filename)
        [rem-backlog
         backlogs
         backlog-summaries
         team-summaries] (work-backlog-iter backlog iterations)]
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
      ;; TODO: convert these # functions into compositions
      (println "Completed " (summarize #(and (progressed :effort
                                                         %1
                                                         %2)
                                             (completed :effort
                                                        %1
                                                        %2))
                                       iter-backlog
                                       backlog-summary))
      (println "Made progress on " (summarize #(and (progressed :effort
                                                                %1
                                                                %2)
                                                    (not
                                                     (completed :effort
                                                                %1
                                                                %2)))
                                              iter-backlog
                                              backlog-summary))
      (println "Left with capacity" (summarize (partial remaining :capacity)
                                               iter-team
                                               team-summary))
      (println))))

(defn -main [& args]
  (let [file (or (first args) "config.edn")]
    (run-and-report file)))
