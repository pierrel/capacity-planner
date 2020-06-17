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

(defn- to-simple-effort
  "Returns an easier-to-work with effort like ([:app 10] [:ios 15])"
  [proj-effort]
  (map vec proj-effort))

(defn- from-simple-effort
  "Takes a simple effort and returns the project effort map"
  [simple-effort]
  (reduce (fn [acc [k v]]
            (assoc acc k v))
          {}
          simple-effort))

(defn- update-simple-effort
  [simple-effort tech effort]
  (conj (remove #(= tech (first %)) simple-effort)
        [tech effort]))

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

(defn update-project
  "Updates the project with a new effort for given tech."
  [project tech effort]
  (let [old-tech-effort (:effort project)
        new-tech-effort (assoc old-tech-effort
                               tech
                               effort)]
    (assoc project
           :effort
           new-tech-effort)))

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
  "Returns [remaining effort, remaining capacity]."
  [capacity tech effort]
  (loop [rem-effort effort
         rem-capacity capacity]
    (let [avail-cap (filter #(has-prof-available? % tech)
                            rem-capacity)
          cur-worker (first avail-cap)]
      (if cur-worker
        (let [cap (:capacity cur-worker)
              cap-left (- cap rem-effort)]
          (if (>= 0 cap-left)
            (recur (- rem-effort (:capacity cur-worker))
                   (update-capacity rem-capacity
                                    (:name cur-worker)
                                    0))
            [0
             (update-capacity rem-capacity
                              (:name cur-worker)
                              cap-left)]))
        [rem-effort rem-capacity]))))


(defn work-on-project
  "Returns the remaining project effort and capacity.

  Capacities are re-ordered to prefer using lower numbers of
  proficiencies first."
  [capacity project]
  (loop [rem-efforts (to-simple-effort (:effort project))
         rem-capacity (sort-by #(-> % :profs count) < capacity)]
    (let [pos-effort (filter (fn [[_ effort]] (< 0 effort)) rem-efforts)
          tech-effort (first pos-effort)
          [tech effort] tech-effort]
      (if (or (nil? tech-effort) ; Nothing left to do
              (empty? (filter #(has-prof-available? % tech)
                              rem-capacity))) ; no one left
        [(reduce (fn [proj [tech effort]]
                   (update-project proj tech effort))
                 project
                 rem-efforts)
         (update-capacities capacity rem-capacity)]
        (let [[rem-effort-num cap-after-work] (work-on-tech rem-capacity
                                                            tech
                                                            effort)]
          (recur (update-simple-effort rem-efforts
                                       tech
                                       rem-effort-num)
                 cap-after-work))))))


(defn work-on
  "Returns [projects' status, remaining capacity] working on projects.

  Finishes when either:
  1) Projects are finished
  2) Capacity is exhausted
  3) There is no more capacity for the given projects"
  [capacity projects]
  (loop [rem-capacity        capacity
         rem-projects        projects
         excluded-proj-names #{}]
    (let [unfinished-projects (filter has-effort? rem-projects)
          doable-projects (remove #(-> (s/intersection excluded-proj-names
                                                       #{(:name %)})
                                       empty? not)
                                  unfinished-projects)
          next-project (first doable-projects)]
      (if (or (nil? next-project)
              (-> rem-capacity have-capacity? not))
        [rem-projects rem-capacity]
        (let [[project cap-after-work] (work-on-project rem-capacity
                                                        next-project)]
          (recur cap-after-work
                 (update-projects rem-projects project)
                 (conj excluded-proj-names (:name project))))))))

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
  [contributions projects constants proficiencies]
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
            results (work-on capacity rem-work)
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

