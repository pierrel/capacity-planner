(ns capacity.core)

(defprotocol Finite
  (exhausted? [x] "Can no longer work or be worked on")
  (diff [before after] "Difference between `before` and `after`"))
(defprotocol Worker
  (doable [x y] "Returns a modified y which can be done by x")
  (work-on [x y] "Returns [x', y'] the result of `x` working on `y`"))
(defprotocol Workable
  (work-out [x y] "returns [x', y'] the result of `y` working on `x`"))

(defn capacity-to-points
  "Returns [capacity, points] after transferring as much capacity to points."
  [capacity points]
  (if (> capacity points)
    [(- capacity points) 0]
    [0 (- points capacity)]))

(defn capacity-to-effort
  "Transfers capacity to each tech's points in turn"
  [capacity effort]
  (reduce (fn [[capacity effort] [tech points]]
            (let [[rem-cap rem-points] (capacity-to-points capacity
                                                           points)]
              [rem-cap
               (assoc effort tech rem-points)]))
          [capacity effort]
          effort))

(defrecord Project [name effort]
  Workable
  (work-out [proj team]
    (reduce (fn [[proj res-team] eng]
              (let [[rem-eng rem-proj] (work-on eng proj)]
                [rem-proj (conj res-team rem-eng)]))
            [proj []]
            team))
  Finite
  (exhausted? [proj]
    (every? zero? (map last (:effort proj))))
  (diff [before after]
    (apply (partial merge-with -)
           (map :effort [after before]))))

(defrecord Eng [name profs capacity]
  Worker
  (doable [eng effort]
    (into {}
          (filter (comp (partial contains? (:profs eng)) first)
                  effort)))
  (work-on [eng proj]
    (let [[rem-capacity
           rem-effort] (capacity-to-effort (:capacity eng)
                                           (doable eng
                                                   (:effort proj)))]
      [(assoc eng :capacity rem-capacity)
       (merge-with merge proj {:effort rem-effort})]))

  Finite
  (exhausted? [eng]
    (-> eng :capacity zero?))
  (diff [before after]
    {:capacity (apply - (map :capacity [after before]))}))

(defn summarize-named
  [befores afters]
  (let [mirror (partition 2 (interleave befores afters))]
    (map #(hash-map :name (-> % first :name)
                    :check (apply = (map :name %))
                    :diff (apply diff %))
         mirror)))

(defn work-backlog
  "Returns [backlog', team'] of the backlog and team after working"
  [backlog team]
  (reduce (fn [[res-backlog rem-team] project]
            (let [[worked-project res-team] (work-out project rem-team)]
              [(conj res-backlog worked-project) res-team]))
          [[] team]
          backlog))

(defn work-backlog-iter
  "Works the backlog over multiple team iterations.

  Returns the remaining backlog (after all team iterations),
          all backlogs (starting with the untouched backlog),
          all backlog summaries (after apply the team),
          all team summaries (after applying to the backlog)
  In that order."
  [backlog iterations]
  (reduce (fn [[rem-backlog backlogs backlog-sums team-sums] team]
            (let [[res-backlog res-team] (work-backlog rem-backlog team)
                  backlog-sum (summarize-named rem-backlog res-backlog)
                  team-sum (summarize-named team res-team)]
              [res-backlog
               (conj backlogs rem-backlog)
               (conj backlog-sums backlog-sum)
               (conj team-sums team-sum)]))
          [backlog [] [] []]
          iterations))
