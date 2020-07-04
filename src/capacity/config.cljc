(ns capacity.config
  (:require [capacity.modeling :as models])
  (:import [capacity.modeling Eng Project]))

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

(defn to-models [file-or-filename]
  (let [conf       (read file-or-filename)
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

;; TODO add some validation for the config file -- maybe use prism?
