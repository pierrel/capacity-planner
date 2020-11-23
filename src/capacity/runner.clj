(ns capacity.runner
  (:require [capacity.config :as config]
            [capacity.utils :as utils]
            [clojure.string :as s])
  (:use [capacity.core]
        [clojure.pprint])
  (:import [capacity.core Eng Project]))

(defn project-lookup
  "Takes a collection of projects and returns a map, keyed by id."
  [coll]
  (zipmap (map id coll) coll))

(defn agg-completion
  "Takes 2 efforts and returns the total percent completion."
  [total current]
  (let [[agg-total agg-current] (map #(apply + (vals %))
                                     [total current])
        total-rec (- agg-total agg-current)]
    (/ total-rec agg-total)))

(defn id-comparison-check
  [& all]
  (let [ids (map id all)]
    (if (not (apply = ids))
      (throw (format "Object ids must match to report on them. Comparing %s"
                     (s/join ", " ids))))))

(defn completion-str
  "Takes 2 projects and returns a string representing their completion."
  [original-project current-project]
  (id-comparison-check original-project current-project)
  (format "%s (%.0f%%)" ;; TODO figure out how not to show decimal places
          (id original-project)
          (* 100 (float
                  (apply agg-completion
                         (map :effort
                              [original-project current-project]))))))

(defn report-change
  [iteration backlog-lookup current-backlog]
  (let [all (map (partial apply completion-str)
                 (map #(list (get backlog-lookup (id %))
                             %)
                      current-backlog))]
    (format "Iteration %d: %s"
            iteration
            (s/join ", " all))))

(defn report-changes
  [backlogs]
  (let [backlog-lookup (-> backlogs first project-lookup)
        after-backlogs (rest backlogs)
        changes-strs (map (partial apply report-change)
                         (utils/group-interleave (range)
                                                 (repeat backlog-lookup)
                                                 after-backlogs))
        final-str (format (if (every? exhausted? (last backlogs))
                            "Backlog complete after %d iterations."
                            "Backlog incomplete after %d iteration.")
                          (count after-backlogs))]
    (format "%s\n\n%s"
            (s/join "\n" changes-strs)
            final-str)))
(defn run
  [filename]
  (let [[backlog iterations] (config/to-models filename)]
    (first (work-backlog-entirely backlog iterations))))

(defn report-backlog
  [backlog]
  (format "Backlog:\n%s"
          (s/join "\n" (map :name backlog))))

(defn run-and-report [filename]
  (let [backlogs (run filename)]
    (format "%s\n\n%s"
            (report-backlog (first backlogs))
            (report-changes backlogs))))

(run-and-report "config.edn")

(defn -main [& args]
  (let [file (or (first args) "config.edn")]
    (println (run-and-report file))))
