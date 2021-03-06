(ns capacity.config
  (:require #?(:clj [capacity.core :as models]
               :cljs [capacity.core :as models :refer [Eng Project]])
            [capacity.validation :as validation]
            [clojure.set :as s])
  #?(:clj (:import [capacity.core Eng Project])))

(def validations
  [;; constants
   {:message "Constants must have sprints and velocity defined"
    :finder :constants
    :validator #(not-any? nil? (map % [:sprints :velocity]))}
   {:message "Constants must have number values"
    :finder (comp vals :constants)
    :validator (partial every? number?)}

   ;; profs
   {:message "profs must be defined as sets"
    :finder (comp vals :profs)
    :validator (partial every? set?)}

   ;; contrib
   {:message "contrib should be a collection of maps"
    :finder :contrib
    :validator #(and (coll? %) (every? map? %))}
   {:message "contrib map values should be numbers"
    :finder #(reduce concat
                     (map vals (:contrib %)))
    :validator (partial every? number?)}

   ;; projects
   {:message "projects should be a collection of maps"
    :finder :projects
    :validator (partial every? map?)}
   {:message "projects should all have a name and effort"
    :finder :projects
    :validator #(not-any? nil? (concat (map :name %)
                                       (map :effort %)))}
   {:message "projects should have effort as a map"
    :finder #(map :effort (:projects %))
    :validator (partial every? map?)}
   {:message "projects should have effort values as numbers"
    :finder (fn [form]
              (reduce concat (map #(-> % :effort vals) (:projects form))))
    :validator (partial every? number?)}
   {:message "projects cannot have effort that is not a proficiency"
    :finder (fn [{profs :profs
                  projects :projects}]
              (map #(apply s/union %)
                   [(map #(-> % :effort keys set)
                          projects)
                    (vals profs)]))
    :validator (partial apply s/subset?)}])

(defn validate
  [form]
  (apply (partial validation/validate form) validations))

(defn make-team
  [contribs profs points]
  (map #(let [name (first %)
              prof (get profs name)
              contrib (last %)]
          (Eng. name prof (* points contrib)))
       contribs))

(defn make-backlog
  [proj-maps]
  (map #(Project. (:name %) (:effort %))
       proj-maps))

(defn read [file-or-filename]
  (with-open [r (clojure.java.io/reader file-or-filename)]
    (clojure.edn/read (java.io.PushbackReader. r))))

(defn to-models
  "Returns the records [`backlog`, `iterations`]

  `backlog` is a list of `Project`
  `iterations` is a list of lists of `Eng`

  `file-filename-or-config` can be a file/filename containing the config or a
  config map."
  [file-filename-or-config]
  (let [conf       (validate
                    (if (map? file-filename-or-config)
                      file-filename-or-config
                      (read file-filename-or-config)))
        const      (:constants conf)
        profs      (:profs conf)
        points     (* (:sprints const)
                      (:velocity const)
                      (- 1 (:unplanned const)))
        backlog    (make-backlog (:projects conf))
        contrib (:contrib conf)
        iterations (map #(make-team % profs points)
                        (if (map? contrib) [contrib] contrib))]
    [backlog iterations]))
