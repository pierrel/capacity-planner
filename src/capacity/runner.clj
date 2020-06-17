(ns capacity.runner
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:use [capacity.core]
        [clojure.pprint]))

(defn read-config [filename]
  (with-open [r (io/reader filename)]
    (edn/read (java.io.PushbackReader. r))))

(defn report-on [result]
  (println "Completed: " (:completed result))
  (println "Made progress on: " (map :name (:progressed result)))
  (println "Left with capacity: " (:remaining-capacity result)))

(defn -main [& args]
  (let [filename (or (first args)
                     "config.edn")
        config (read-config filename)
        projects(:projects config)
        const (:constants config)
        results (work-on-long (:contrib config)
                              projects
                              const
                              (:profs config))]
    (doseq [[iter result] (partition 2 (interleave (rest (range)) results))]
      (println "Iteration " iter)
      (report-on result)
      (println))))
