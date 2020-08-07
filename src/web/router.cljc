(ns web.router
  (:require [clojure.string :as s]))

(def param-capture-pattern "[^/]+")
(def param-re #"\{([^}]+)\}")

(defn route-to-regex
  [route]
  (re-pattern (s/replace route
                         param-re
                         (str "(" param-capture-pattern ")"))))

(defn route-to-params
  [route]
  (map (comp keyword last)
       (re-seq param-re route)))

(defn to-route
  [route-str action]
  {:def route-str
   :re (route-to-regex route-str)
   :params (route-to-params route-str)
   :action action})

(defn apply-route
  [route uri-params]
  (let [action (:action route)
        params (:params route)]
    (if (fn? action)
      (action (apply hash-map (interleave params uri-params)))
      action)))

(defn matching-route-params
  "Returns [route, params] of the first matching route and uri params."
  [routes uri]
  (first (filter (complement nil?)
                 (map #(if-let [match (re-matches (:re %) uri)]
                         [%
                          (rest match)])
                      routes))))

(defn routes
  "Returns the response based on uri match.

  It's currently very slow since it has to construct the associations each time
  it's called. Need to store it somewhere in the future. Probably need to use a
  macro."
  [uri & route-resp-pairs]
  (let [default (last route-resp-pairs)
        route-resps (map #(apply to-route %)
                        (partition 2 (butlast route-resp-pairs)))
        route-params (matching-route-params route-resps uri)]
    (if route-params
      (apply apply-route route-params)
      (run default))))
