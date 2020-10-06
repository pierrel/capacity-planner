(ns capacity.report
  (:require [capacity.utils :as utils]))

;; TODO: Need to re-write these not to take "key"
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
;; TODO: Remove the "struct" from these versions and refactor the non-struct version to have something like "formatted" or "str"
(defn summarize-struct
  "A structured summary"
  [filt original summary]
  (filter #(apply filt %)
          (utils/group-interleave original summary)))

(defn full-summary-struct
  "A structured full summary"
  [iter-backlog backlog-summary iter-team team-summary]
  {:completed (summarize-struct #(and (progressed :effort
                                                  %1
                                                  %2)
                                      (completed :effort
                                                 %1
                                                 %2))
                                iter-backlog
                                backlog-summary)
   :progressed (summarize-struct #(and (progressed :effort
                                                   %1
                                                   %2)
                                       (not
                                        (completed :effort
                                                   %1
                                                   %2)))
                                 iter-backlog
                                 backlog-summary)
   :leftover (summarize-struct (partial remaining :capacity)
                               iter-team
                               team-summary)})

(defn format-summary [summary]
  (format "%s (%.2f)"
          (:name summary)
          (-> summary :ratio double)))

(defn summarize
  [filt original summary]
  (map #(-> % last format-summary)
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
