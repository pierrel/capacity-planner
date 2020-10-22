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


