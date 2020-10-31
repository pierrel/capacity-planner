(ns capacity.cp
  (:require [capacity.config :as config]
            [capacity.core :as core]
            [capacity.utils :as utils]
            [capacity.report :as report]
            [clojure.set :as s])
  (:import [com.google.ortools Loader]
           [com.google.ortools.linearsolver
            MPConstraint
            MPObjective
            MPSolver
            MPVariable]))

;; Use example from https://developers.google.com/optimization/lp/glop

(def conf
  ;; Simple set-up to test out or-tools
  {:constants {:sprints   1
               :unplanned 0
               :velocity  10} ; So contrib of 1 = 10 total capacity
   :profs     {:pierre #{:app :web :android}
               :ana #{:app :ios}}
   :contrib   [{:pierre 1
                :ana 1}]
   :projects  [{:name   "Athena"
                     :effort {:app 11
                              :web 3
                              :ios 2}}]})


(defn do-maps
  "Performs `dofn` on each path in nested-maps.

  For example, for {:a {:b 1 :c 2} :d 3} does:
  (dofn [:a :b 1])
  (dofn [:a :c 2])
  (dofn [:d 3])"
  [dofn nested-maps & [path]]
  (loop [keys-left (keys nested-maps)]
    (if (not (empty? keys-left))
      (let [k (first keys-left)
            v (get nested-maps k)
            final-path (if (nil? path)
                         [k]
                         (conj path k))]
        (if (map? v)
          (do
            (do-maps dofn v final-path)
            (recur (rest keys-left)))
          (do
            (dofn (conj final-path v))
            (recur (rest keys-left))))))))

(defn map-to-paths
  [nested-maps & [path paths]]
  (loop [keys-left (keys nested-maps)
         all-paths (if (nil? paths) [] paths)]
    (if (not (empty? keys-left))
      (let [k (first keys-left)
            v (get nested-maps k)
            current-path (if (nil? path)
                           [k]
                           (conj path k))]
        (recur (rest keys-left)
               (if (map? v)
                 (map-to-paths v current-path all-paths)
                 (conj all-paths (conj current-path v)))))
      all-paths)))

(defn map-maps
  "Performs `f` on each leaf (non-map) of `maps` and returns a new map with
  leaves replaced by the results."
  [f maps]
  (loop [paths (map-to-paths maps)
         acc maps]
    (if (empty? paths)
      acc
      (let [path (first paths)]
        (recur (rest paths)
               (update-in acc
                          (butlast path)
                          (fn [_]
                            (f path))))))))

(defn constraint-name
  [eng skill]
  (format "%s-%s" eng skill))

(defn per-eng-skill-vars
  "Creates variables one per eng-skill and puts it in a map of maps.

  Looks like the following:
  {:abe {:ios [variable] :app [variable]}
   :jen {:ios [variable] :app [variable]}}

  Does not create variables for which there is no need (no project effort)."
  [solver team project]
  (utils/with-lookup [effort-lookup (-> project :effort keys)]
    (reduce (fn [acc cur]
              (assoc acc
                     (:name cur)
                     (apply assoc {}
                            (let [profs (filter effort-lookup
                                                (:profs cur))]
                              (interleave profs
                                          (map #(.makeNumVar solver
                                                             0.0
                                                             (:capacity cur)
                                                             (variable-name
                                                              (:name cur)
                                                              %))
                                               profs))))))
            {}
            team)))

(let [[backlog iterations] (config/to-models conf)
      team (first iterations)
      project (first backlog)
      effort (:effort project)
      skills (keys effort)]
  (Loader/loadNativeLibraries)
  (let [solver (MPSolver/createSolver "GLOP")
        variables (per-eng-skill-vars solver team project)
        skill-constraints (map #(.makeConstraint solver
                                                 0.0
                                                 (get effort %)
                                                 (name %))
                               skills)
        capacity-constraints (map #(.makeConstraint solver
                                                    0.0
                                                    (:capacity %)
                                                    (-> % :name name))
                              team)]
    (println "\nclear")
    ;; Set skill coefficients
    (println "skills")
    (doseq [constraint skill-constraints]
      (let [skill (keyword (.name constraint))]
        (do-maps (fn [[eng-name vskill variable]]
                   (println (format "Setting coef of %s and %s of const %s to %d"
                                    eng-name
                                    vskill
                                    (.name constraint)
                                    (if (= skill vskill)
                                      1
                                      0)))
                   (.setCoefficient constraint
                                    variable
                                    (if (= vskill skill)
                                      1
                                      0)))
                 variables)))
    ;; Set the capacity coefficients
    (println "capacity")
    (doseq [constraint capacity-constraints]
      (let [eng-name (keyword (.name constraint))]
        (do-maps (fn [[veng-name skill variable]]
                   (println (format "Setting coef of %s and %s of const %s to %d"
                                    veng-name
                                    skill
                                    (.name constraint)
                                    (if (= veng-name eng-name)
                                      1
                                      0)))
                   (.setCoefficient constraint
                                    variable
                                    (if (= veng-name eng-name)
                                      1
                                      0)))
                 variables)))
    ;; Objective is to maximize sum of all variables
    (let [objective (.objective solver)]
      (do-maps (fn [[_ _ variable]]
                 (.setCoefficient objective
                                  variable
                                  1))
               variables)
      (.setMaximization objective))
    ;; Solve!
    [(:effort project)
     team
     (let [solution (.solve solver)]
       (map-maps (fn [[eng skill variable]]
                   (.solutionValue variable))
                 variables))]))


;; run the original solver
(let [[backlog iterations] (config/to-models conf)
      [remaining-backlog
       backlogs
       backlog-summaries
       team-summaries] (core/work-backlog-iter backlog iterations)]
  remaining-backlog)


