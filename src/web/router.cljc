(ns web.router
  (:require [clojure.string :as s]))

(defn run
  [resp]
  (if (fn? resp)
    (resp) ;; will add params later
    resp))

(defn routes
  "Returns the response based on uri match.

  It's currently very slow since it has to construct the association each time
  it's called. Need to store it somewhere in the future."
  [uri & route-resp-pairs]
  (let [default (last route-resp-pairs)
        route-resp (apply hash-map (butlast route-resp-pairs))
        match (get route-resp uri)]
    (run (if match
           match
           default))))
