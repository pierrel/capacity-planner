(ns capacity.cp
  (:require [capacity.config :as config]
            [capacity.utils :as utils])
  (:import [com.google.ortools Loader]
           [com.google.ortools.linearsolver
            MPConstraint
            MPObjective
            MPSolver
            MPVariable]))

(Loader/loadNativeLibraries)

(def sample-conf
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

(defn do-maps
  "Performs `dofn` on each path in nested-maps.

  For example, for {:a {:b 1 :c 2} :d 3} does:
  (dofn [:a :b 1])
  (dofn [:a :c 2])
  (dofn [:d 3])"
  [dofn nested-maps & [path]]
  (doseq [path (map-to-paths nested-maps)]
    (dofn path)))


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

(defn named
  [eng skill]
  (format "%s-%s" eng skill))

(defn per-eng-skill-vars
  "Creates variables one per eng-skill and puts it in a map of maps.

  Looks like the following:
  {:abe {:ios [variable] :app [variable]}
   :jen {:ios [variable] :app [variable]}}

  Does not create variables for which there is no need (no project effort)."
  [solver team effort]
  (utils/with-lookup [effort-lookup (keys effort)]
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
                                                             (named
                                                              (:name cur)
                                                              %))
                                               profs))))))
            {}
            team)))

(defn solve
  "Solves for `team` and `effort` by maximizing per-proficiency capacity.

  Result is the change, per proficiency to each engineer:
  {:pierre {:ios 10 :app 3}
  :other {:android 3 :app 1}}"
  [effort team]
  (let [solver (MPSolver/createSolver "GLOP")
        variables (per-eng-skill-vars solver team effort)
        skill-constraints (map #(.makeConstraint solver
                                                 0.0
                                                 (get effort %)
                                                 (name %))
                               (keys effort))
        capacity-constraints (map #(.makeConstraint solver
                                                    0.0
                                                    (:capacity %)
                                                    (-> % :name name))
                                  team)]
    ;; Set skill coefficients
    (doseq [constraint skill-constraints]
      (let [skill (keyword (.name constraint))]
        (do-maps (fn [[eng-name vskill variable]]
                   (.setCoefficient constraint
                                    variable
                                    (if (= vskill skill)
                                      1
                                      0)))
                 variables)))
    ;; Set the capacity coefficients
    (doseq [constraint capacity-constraints]
      (let [eng-name (keyword (.name constraint))]
        (do-maps (fn [[veng-name skill variable]]
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
    (let [solution (.solve solver)]
      (map-maps (fn [[eng skill variable]]
                  (.solutionValue variable))
                variables))))

(defn -main [& args]
  (let [[backlog iterations] (config/to-models sample-conf)
        team (first iterations)
        effort (-> backlog first :effort)]
    (println (solve effort
                    team))))
