(ns capacity.core
  (:require [clojure.set :as s]
            [capacity.utils :as utils]))

(defn team
  "Returns a list of all engineers with capacity and proficiencies"
  [contrib profs points]
  (map (fn [[person contib]]
         {:name person
          :profs (get profs person)
          :capacity (* points (get contrib person))})
       (remove #(= 0 (last %)) contrib)))

(defn update-teammate-capacity
  "Takes a `team` updates the eng of `name` with `new-cap`."
  [team name new-cap]
  (map #(if (= (:name %) name)
          (assoc % :capacity new-cap)
          %)
       team))

(defn update-effort
  "Updates the project with a new effort."
  [project effort]
  (merge-with merge
              project
              {:effort effort}))

(defn has-effort? [project]
  (some (partial < 0) (-> project :effort vals)))

(defn has-capacity? [eng]
  (< 0 (:capacity eng)))

(defn has-prof-available?
  "Returns true if the engineer has the proficiency and positive capacity."
  [eng tech]
  (and (< 0 (:capacity eng))
       (not (empty? (s/intersection #{tech} (:profs eng))))))

(defn eng-work-on
  [eng points]
  (let [cap (:capacity eng)]
    (if (> points cap)
      [(assoc eng :capacity 0) (- points cap)]
      [(assoc eng :capacity (- cap points)) 0])))

(defn team-work-on-tech
  "Returns [remaining team, remaining points]."
  [team tech points]
  (reduce (fn [[rem-team rem-points] eng]
            (let [[worked-eng worked-points] (eng-work-on eng rem-points)]
              [(update-teammate-capacity rem-team
                                         (:name worked-eng)
                                         (:capacity worked-eng))
               worked-points]))
          [team points]
          (filter #(has-prof-available? % tech) team)))

(defn work-on-project
  "Returns the remaining project effort and team."
  [project team]
  (let [[rem-effort rem-team]
        (reduce (fn [[effort team] [tech points]]
                  (let [[rem-team rem-points] (team-work-on-tech team
                                                                 tech
                                                                 points)]
                    [(assoc effort tech rem-points) rem-team]))
                [{} (sort-by #(-> % :profs count) team)]
                (:effort project))]
    [(update-effort project rem-effort)
     (utils/sort-like team :name rem-team)]))

(defn work-on
  "Returns [projects' status, remaining team] working on projects."
  [projects team]
  (reduce (fn [[projects team] project-todo]
            (let [[rem-project rem-team] (work-on-project project-todo
                                                          team)]
              [(conj projects rem-project)
               rem-team]))
          [[] team]
          projects))

(defn work-summary
  [projects projects-after-work team-after-work]
  (let [mirrored (partition 2 (interleave projects-after-work
                                          projects))
        progressed (filter has-effort?
                           (map first
                                (remove #(apply = (map :effort %))
                                        mirrored)))
        complete-projects (filter (complement has-effort?)
                                  projects-after-work)]
    {:completed (map :name complete-projects)
     :progressed progressed
     :remaining-team (filter has-capacity? team-after-work)}))

(defn work-on-long
  [projects contributions constants proficiencies]
  "Works on projects each team in turn"
  (let [points (* (:velocity constants)
                  (:sprints constants)
                  (- 1 (:unplanned constants)))]
    (first (reduce (fn [[summary projects] capacity]
                     (let [[rem-projects rem-capacity]
                           (work-on projects capacity)]
                       [(conj summary
                              (work-summary projects rem-projects rem-capacity))
                        (filter has-effort? rem-projects)]))
                   [[] projects]
                   (map #(team %
                                        proficiencies
                                        points)
                        contributions)))))

