(ns capacity.core
  (:require [clojure.set :as s]
            [capacity.utils :as utils]))

(defn team-capacity
  "Returns a list of all engineers with capacity and proficiencies"
  [contrib profs points]
  (map (fn [[person contib]]
         {:name person
          :profs (get profs person)
          :capacity (* points (get contrib person))})
       (remove #(= 0 (last %)) contrib)))

(defn update-capacities
  "Takes all the capacities in `new` and updates them in `original`.

  Because we may need to re-order capacities, this allows us to maintain
  order before returning."
  [original new]
  (map (fn [orig]
         (if-let [new-matching (first (filter #(= (:name %) (:name orig))
                                              new))]
           (assoc orig :capacity (:capacity new-matching))
           orig))
       original))

(defn update-capacity
  "Takes a `capacities` list of capacity and updates it with `updated`."
  [capacities name new-cap]
  (map #(if (= (:name %) name)
          (assoc % :capacity new-cap)
          %)
       capacities))

(defn update-effort
  "Updates the project with a new effort."
  [project effort]
  (merge-with merge
              project
              {:effort effort}))

(defn update-projects
  "Replaces project of the same name with the new project."
  [orig-projects new-project]
  (utils/replace-with (fn [np op]
                        (apply = (map :name [np op])))
                      orig-projects
                      new-project))

(defn has-effort? [project]
  (some (partial < 0) (-> project :effort vals)))

(defn have-effort? [projects]
  (some has-effort? projects))

(defn has-capacity? [capacity]
  (< 0 (:capacity capacity)))

(defn have-capacity? [capacities]
  (some (partial < 0) (map :capacity capacities)))

(defn has-prof-available?
  "Returns true if the engineer has the proficiency and positive capacity."
  [eng-cap tech]
  (and (< 0 (:capacity eng-cap))
       (not (empty? (s/intersection #{tech} (:profs eng-cap))))))

(defn work-on-tech
  "Returns [remaining capacity, remaining points]."
  [capacity tech points]
  (loop [rem-points points
         rem-capacity capacity]
    (let [avail-cap (filter #(has-prof-available? % tech)
                            rem-capacity)
          cur-worker (first avail-cap)]
      (if cur-worker
        (let [cap (:capacity cur-worker)
              cap-left (- cap rem-points)]
          (if (>= 0 cap-left)
            (recur (- rem-points (:capacity cur-worker))
                   (update-capacity rem-capacity
                                    (:name cur-worker)
                                    0))
            [(update-capacity rem-capacity
                              (:name cur-worker)
                              cap-left)
             0]))
        [rem-capacity rem-points]))))


(defn work-on-project
  "Returns the remaining project effort and capacity.

  Capacities are re-ordered to prefer using lower numbers of
  proficiencies first."
  [project capacity]
  (let [[rem-effort rem-capacity]
        (reduce (fn [[effort capacity] [tech points]]
                  (let [[rem-capacity rem-points] (work-on-tech capacity
                                                                tech
                                                                points)]
                    [(assoc effort tech rem-points) rem-capacity]))
                [{} (sort-by #(-> % :profs count) capacity)]
                (:effort project))]
    [(update-effort project rem-effort)
     (update-capacities capacity rem-capacity)]))

(defn work-on
  "Returns [projects' status, remaining capacity] working on projects."
  [projects capacity]
  (reduce (fn [[projects capacity] project-todo]
            (let [[rem-project rem-capacity] (work-on-project project-todo
                                                              capacity)]
              [(conj projects rem-project)
               rem-capacity]))
          [[] capacity]
          projects))

(defn work-summary
  [projects projects-after-work remaining-capacity]
  (let [mirrored (partition 2 (interleave projects-after-work
                                          projects))
        progressed (filter has-effort?
                           (map first
                                (remove #(= (-> % first :effort)
                                            (-> % last :effort))
                                        mirrored)))
        complete-projects (filter (complement has-effort?)
                                  projects-after-work)]
    {:completed (map :name complete-projects)
     :progressed progressed
     :remaining-capacity (filter has-capacity? remaining-capacity)}))

(defn work-on-long
  [projects contributions constants proficiencies]
  "Works on projects each capacity in turn"
  (let [points (* (:velocity constants)
                  (:sprints constants)
                  (- 1 (:unplanned constants)))]
    (loop [rem-work projects
           rem-contributions contributions
           summaries []] ; {:complete [] :progress [] :remaining-capacity ()}
      (let [capacity (team-capacity (first rem-contributions)
                                    proficiencies
                                    points)
            results (work-on rem-work capacity)
            [project-status cap-left-over] results
            next-summaries (conj summaries
                                 (work-summary rem-work
                                               project-status
                                               cap-left-over))
            next-contribution (rest rem-contributions)]
        (if (or (empty? next-contribution)
                (not (have-effort? project-status)))
          next-summaries
          (recur (filter has-effort? project-status)
                 next-contribution
                 next-summaries))))))

