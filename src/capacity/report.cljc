(ns capacity.report
  (:require [capacity.utils :as utils]))

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
  (map #(str (-> % first :name)
             " "
             (-> % last :diff))
       (filter #(apply filt %)
               (utils/group-interleave original summary))))

(defn full-summary
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
