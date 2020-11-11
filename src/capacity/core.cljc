(ns capacity.core
  (:require [capacity.utils :as utils]
            [capacity.cp :as cp]))

(defprotocol Finite
  (diff [before after] "Difference between `before` and `after`")
  (ratio [before after] "Total ratio of what was done between `before` and `after`")
  (exhausted? [x] "Returns true if the object can no longer work or be worked on."))
(defprotocol Workable
  (work-out [x y] "returns [x', y'] the result of `y` working on `x`"))
(defprotocol Identifiable
  (id [x] "returns the uniquely identifiable attribute of `x`"))

(defn merge-cp-solution
  "Takes the `solution` output of cp/solve and returns a modified team and
  project."
  [solution project team]
  (let [effort-change (apply (partial merge-with +)
                             (vals solution))
        capacity-change (zipmap (keys solution)
                                (map #(reduce + (vals %))
                                     (vals solution)))]
    [(assoc project
            :effort
            (merge-with - (:effort project) effort-change))
     ;; TODO: make less awkward
     (map #(assoc %
                  :capacity
                  (- (:capacity %)
                     (or (get capacity-change
                              (:name %))
                         0)))
          team)]))

(defrecord Project [name effort]
  Workable
  (work-out [proj team]
    (merge-cp-solution (if (or (exhausted? proj)
                               (every? exhausted? team))
                         {}
                         (cp/solve (:effort proj)
                                   team))
                       proj
                       team))

  Finite
  (diff [before after]
    (apply (partial merge-with -)
           (map :effort [after before])))
  (ratio [before after]
    (let [[after-total-effort before-total-effort]
          (map #(apply + (->  % :effort vals))
               [after before])]
      (if (zero? before-total-effort)
        0
        (- 1 (/ after-total-effort before-total-effort)))))
  (exhausted? [proj]
    (every? (comp not pos?) (vals (:effort proj))))

  Identifiable
  (id [x]
    (:name x)))

(defrecord Eng [name profs capacity]
  Finite
  (diff [before after]
    {:capacity (apply - (map :capacity [after before]))})
  (ratio [before after]
    (let [[after-cap before-cap] (map :capacity [after before])]
      (if (zero? before-cap)
        0
        (- 1 (/ after-cap before-cap)))))
  (exhausted? [eng]
    (zero? (:capacity eng)))

  Identifiable
  (id [eng]
    (:name eng)))

;; name    The identifiable part of the record
;; check   True or false depending on whether the id matches
;; diff    The diff of before and after
;; ratio   Ratio change between after and original
(defrecord Change [name check diff ratio])

(defn summarize
  "Returns the Change between the two states and an original."
  [before after original]
  (map->Change {:name (id before)
                :check (apply = (map id [before after original]))
                :diff (diff before after)
                :ratio (ratio original after)}))

(defn summarize-all
  "Returns a summary of the changes between two states and an original.

  Takes 3 lists of Identifiable Finite records and returns a Change for each."
  [befores afters originals]
  (map (partial apply summarize)
       (utils/group-interleave befores afters originals)))

(defn work-backlog
  "Returns [backlog', team'] of the backlog and team after working"
  [backlog team]
  (reduce (fn [[res-backlog rem-team] project]
            (let [[worked-project res-team] (work-out project rem-team)]
              [(conj res-backlog worked-project) res-team]))
          [[] team]
          backlog))

(defn work-backlog-iter
  "Works the backlog over multiple team iterations until either the backlog or
  iterations are exhausted.

  Returns the remaining backlog (after all team iterations),
          all backlog iterations (starting with the untouched backlog),
          all backlog summaries (after apply the team),
          all team summaries (after application to the backlog)
  In that order."
  [backlog iterations]
  (loop [rem-backlog backlog
         backlogs []
         backlog-sums []
         team-sums []
         rem-teams iterations]
    (if (or (empty? rem-teams)
            (every? exhausted? rem-backlog))
      [rem-backlog
       backlogs
       backlog-sums
       team-sums]
      (let [team (first rem-teams)
            [res-backlog res-team] (work-backlog rem-backlog team)
            backlog-sum (summarize-all rem-backlog res-backlog backlog)
            team-sum (summarize-all team res-team team)]
        (recur res-backlog
               (conj backlogs rem-backlog)
               (conj backlog-sums backlog-sum)
               (conj team-sums team-sum)
               (rest rem-teams))))))

(defn work-backlog-entirely
  "Works the backlog over all team iterations and then continues with the final
  iteration until the backlog is exhausted.

  Returns the remaining backlog (after all team iterations),
          all backlog iterations (starting with the untouched backlog),
          all backlog summaries (after apply the team),
          all team summaries (after application to the backlog)
  In that order."
  [backlog iterations]
  (work-backlog-iter backlog
                     (concat iterations
                             (-> iterations last repeat))))
