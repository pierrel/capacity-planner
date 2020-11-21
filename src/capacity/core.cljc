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

  Returns all backlog iterations (starting with the untouched backlog),
          all used team iterations before use
          all used team iterations after use
  In that order."
  [backlog iterations]
  (loop [rem-backlog backlog
         backlogs [backlog]
         teams-before []
         teams-after []
         rem-teams iterations]
    (if (or (empty? rem-teams)
            (every? exhausted? rem-backlog))
      [backlogs teams-before teams-after]
      (let [team-before (first rem-teams)
            [res-backlog team-after] (work-backlog rem-backlog team-before)]
        (recur res-backlog
               (conj backlogs res-backlog)
               (conj teams-before team-before)
               (conj teams-after team-after)
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
