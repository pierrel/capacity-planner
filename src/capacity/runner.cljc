(ns capacity.runner
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:use [capacity.core]
        [clojure.pprint]))

(defn read-config [filename]
  (with-open [r (io/reader filename)]
    (edn/read (java.io.PushbackReader. r))))

(defn -main [& args]
  (let [filename (or (first args)
                     "config.edn")
        config (read-config filename)
        projects(:projects config)
        const (:constants config)
        points (* (:velocity const)
                  (:sprints const)
                  (- 1 (:unplanned const)))
        team (team-capacity (:contrib config)
                            (:profs config)
                            points)
        [res-projects res-capacity] (work-on team projects)]
    (println "Starting with")
    (pprint projects)
    (pprint team)
    (println "Then")
    (pprint res-projects)
    (pprint res-capacity)))

