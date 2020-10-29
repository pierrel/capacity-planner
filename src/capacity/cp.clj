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
   :profs     {:pierre #{:app :web}
               :ana #{:app :ios}}
   :contrib   [{:pierre 1
                :ana 1}]
   :projects  [{:name   "Athena"
                     :effort {:app 11
                              :web 3
                              :ios 2}}]})

(defn per-eng-skill-vars
  "Creates variables one per eng-skill and puts it in a map of maps.

  Looks like the following:
  {:abe {:ios [variable] :app [variable]}
   :jen {:ios [variable] :app [variable]}}"
  [solver eng-names skills]
  (zipmap eng-names
          (map (fn [eng-name]
                 (zipmap skills
                         (map (fn [skill]
                                (.makeNumVar solver
                                             0.0
                                             ##Inf
                                             (format "%s-%s"
                                                     eng-name
                                                     skill)))
                              skills)))
               eng-names)))

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

(defn all-paths
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
                 (all-paths v current-path all-paths)
                 (conj all-paths (conj current-path v)))))
      all-paths)))

(defn map-maps
  "Performs `dofn` on each leaf (non-map) of `maps` and returns a new map with
  leaves replaced by the results."
  [dofn maps]
  (loop [paths-left paths ;; need to calculate paths
         res maps]
    (let [path (first paths-left)]
      (if (nil? path)
        res
        (recur (rest paths-left)
               (update-in res dofn path))))))

(let [[backlog iterations] (config/to-models conf)
      team (first iterations)
      project (first backlog)
      skills (apply s/union (map :profs team))
      skill-max (:effort project)]
  (Loader/loadNativeLibraries)
  (let [solver (MPSolver/createSolver "GLOP")
        eng-names (map :name team)
        variables (per-eng-skill-vars solver eng-names skills)
        skill-constraints ()]

    ))

;; run the new solver one variable per engineer
;; THIS DOESNT WORK - MUST TRY ONE VARIABLE PER ENG-PROF
(let [[backlog iterations] (config/to-models conf)
      team (first iterations)
      project (first backlog)
      skills (apply s/union (map :profs team))
      skill-max (:effort project)
      ]
  (Loader/loadNativeLibraries)
  ;; Add variables
  (let [solver (MPSolver/createSolver "GLOP")
        eng-names (map #(name (get % :name)) team)
        skill-names (map name skills)
        variables (zipmap eng-names
                          (map #(.makeNumVar solver
                                             0.0
                                             ##Inf
                                             %)
                               eng-names))
        skill-constraints (zipmap skill-names
                                  (map #(.makeConstraint solver
                                                         0.0
                                                         (get skill-max
                                                              (keyword %))
                                                         %)
                                       skill-names))
        capacity-constraints (zipmap eng-names
                                     (map
                                      #(.makeConstraint solver
                                                        0.0
                                                        (get % :capacity)
                                                        (name (get % :name)))
                                      team))]
    ;; Set the capacity coefficients
    (doseq [eng-name eng-names]
      (let [constraint (get capacity-constraints eng-name)
            variable (get variables eng-name)]
        (doseq [in-eng-name eng-names]
          (.setCoefficient constraint
                           variable
                           (if (= eng-name in-eng-name)
                             1
                             0)))))

    ;; Set the skill coefficients
    (doseq [skill-name skill-names]
      (let [skill-constraint (get skill-constraints skill-name)]
        (doseq [eng team]
          (let [variable (get variables (name (:name eng)))
                prof-lookup (utils/to-lookup (:profs eng))]
            (.setCoefficient skill-constraint
                             variable
                             (if (get prof-lookup (keyword skill-name))
                               1
                               0))))))
    ;; Objective is to maximize the sum of all efforts/spent capacities
    (let [objective (.objective solver)]
      (doseq [variable (vals variables)]
        (.setCoefficient objective variable 1))
      (.setMaximization objective))
    (let [solution (.solve solver)
          varResults (zipmap (map keyword (keys variables))
                             (map #(.solutionValue %)
                                  (vals variables)))]
      varResults)))

;; For just checking things
(let [[backlog iterations] (config/to-models conf)
      team (first iterations)]
  team)


;; Add skill-level constraints



;; run the original solver
(let [[backlog iterations] (config/to-models conf)
      [remaining-backlog
       backlogs
       backlog-summaries
       team-summaries] (core/work-backlog-iter backlog iterations)]
  remaining-backlog)


