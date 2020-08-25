(ns web.templates
  (:require [clojure.string :as s]))

(defn with-common-js [& urls]
  (vec (map (fn [arg]
              [:script {:type "text/javascript" :src arg}])
            urls)))

(def prod-js
  (with-common-js "js/main.js"))

(def dev-js
  (conj (with-common-js "goog/base.js" "repl.js")
        [:script {:type "text/javascript"} "goog.require('web.js.repl');"]))

(defn- javascript [dev?]
  (if dev? dev-js prod-js))

(defn section
  [form]
  [:div form])

(defn input
  ([input-type label name value]
   (let [id (str (s/lower-case label) "-" input-type "-" name)]
     [:p
      [:label {:for id} label]
      [:input {:type input-type
               :name name
               :id id
               :value value}]]))
  ([input-type label value]
   (input input-type label (s/lower-case label) value)))

(javascript true)

(defn template [form]
  (into [:html
         (if (-> form first vector?)
           (into [:body] form)
           [:body form])]
        (javascript true)))
