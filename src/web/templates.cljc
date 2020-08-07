(ns web.templates
  (:require [clojure.string :as s]))

(defn section
  [form]
  [:div form])

(defn input
  [input-type label value]
  (let [name (s/lower-case label)
        id (str (s/lower-case label) "-" input-type)]
    [:p
     [:label {:for id} label]
     [:input {:type input-type
              :name name
              :id id
              :value value}]]))

(defn template [form]
  [:html
   (if (-> form first vector?)
     (into [:body] form)
     [:body form])])
