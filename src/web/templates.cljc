(ns web.templates
  (:require [clojure.string :as s]))

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

(defn template [form]
  [:html
   (if (-> form first vector?)
     (into [:body] form)
     [:body form])])
