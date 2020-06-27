(ns capacity.modeling)

(defprotocol Finite
  (exhausted? [x] "Can no longer work or be worked on"))
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
            team)))

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
       (merge-with merge proj {:effort rem-effort})])))


(defn work-backlog
  "Returns [backlog', team'] of the backlog and team after working"
  [backlog team]
  (reduce (fn [[res-backlog rem-team] project]
            (let [[worked-project res-team] (work-out project rem-team)]
              [(conj res-backlog worked-project) res-team]))
          [[] team]
          backlog))
