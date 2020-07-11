(ns capacity.runner
  (:require [capacity.config :as config]
            [capacity.utils :as utils]
            [clojure.string :as s])
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

(defn report-map
  [iter-backlog backlog-summary iter-team team-summary]
  {:completed (summarize #(and (progressed :effort
                                           %1
                                           %2)
                               (completed :effort
                                          %1
                                          %2))
                         iter-backlog
                         backlog-summary)
   :progressed (summarize #(and (progressed :effort
                                            %1
                                            %2)
                                (not
                                 (completed :effort
                                            %1
                                            %2)))
                          iter-backlog
                          backlog-summary)
   :leftover (summarize (partial remaining :capacity)
                        iter-team
                        team-summary)})

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
  (report-str iter (report-map iter-backlog
                               backlog-summary
                               iter-team
                               team-summary)))

(defn run-and-report [filename]
  (let [[backlog iterations] (config/to-models filename)
        [rem-backlog
         backlogs
         backlog-summaries
         team-summaries] (work-backlog-iter backlog iterations)]
    (s/join "\n" (map #(apply report %)
                      (utils/group-interleave (range)
                                              backlogs
                                              backlog-summaries
                                              iterations
                                              team-summaries)))))

(defn -main [& args]
  (let [file (or (first args) "config.edn")]
    (println (run-and-report file))))
