(ns web.views.config
  (:require [web.templates :as t]
            [capacity.utils :as utils]
            [capacity.config :as config]
            [capacity.core :as core]
            [clojure.set :as st]
            [clojure.string :as s]
            [clojure.edn :as edn]))

(defn render-summary-item
  [heading lst]
  [:div
   [:h2 heading]
   [:ul
    (map #(vector :li %) lst)]])

(defn render-summary [summary]
  (map #(render-summary-item (-> % first name) (last %)) summary))

(defn render-constants-inputs
  [sprints unplanned velocity]
  (into [:fieldset
         [:legend "Constants"]]
        (map (partial apply t/input)
             [["input" "Sprints" "constants[sprints]" sprints]
              ["input" "Unplanned" "constants[unplanned]" unplanned]
              ["input" "Velocity" "constants[velocity]" velocity]])))

(defn render-profs-inputs
  [eng-profs]
  (into [:fieldset
         [:legend "Proficiencies"]]
        (map #(let [[eng profs] %]
                (t/input "input"
                         (name eng)
                         (format "profs[%s]" (name eng))
                         (s/join ", " (map name profs))))
             eng-profs)))

(defn render-contrib-iter-inputs
  [iteration contrib-iter all-engs]
  (into (list [:input {:type "submit"
                       :name (format "remove[contrib][%d]" iteration)
                       :value "Remove"}])
        (map #(let [eng %
                    eng-name (name eng)]
                (t/input "input"
                         eng-name
                         (format "contrib[%d][%s]"
                                 iteration
                                 eng-name)
                         (get contrib-iter eng 0)))
             all-engs)))

(defn render-contrib-iter
  [iteration contrib-iter all-engs]
  (into [:fieldset
         [:legend (str "Iteration " iteration)]]
        (render-contrib-iter-inputs iteration contrib-iter all-engs)))

(defn render-contribs
  [contribs all-engs]
  (into [:fieldset
         [:legend "Contributions"]]
        (map (partial apply render-contrib-iter)
             (utils/group-interleave (range)
                                     contribs
                                     (repeat all-engs)))))

(defn render-prof-val
  [project-name prof value]
  (t/input "input"
           (format "%s effort" prof)
           (format "projects[%s][effort][%s]"
                   project-name
                   prof)
           value))

(defn render-effort
  [project-name effort all-profs]
  (map #(render-prof-val project-name
                         (name %)
                         (get effort % 0))
       all-profs))

(defn render-project
  [number project all-profs]
  (let [{name :name} project]
    [:fieldset.project name
     (if (nil? number)
       [:span]
       (t/input "input"
                "Rank"
                (format "projects[%s][rank]" name)
                number))
     (t/input "input"
              "Name"
              (format "projects[%s][name]" name)
              name)
     (render-effort name (:effort project) all-profs)
     [:input {:type "submit"
              :name (format "remove[projects][%d]" number)
              :value "Remove"}]]))

(defn param-to-project
  [project-params]
  {:name (get project-params "name")
   :effort (let [effort (get project-params "effort")]
             (zipmap (map keyword
                          (keys effort))
                     (map edn/read-string
                          (vals effort))))})

(defn params-to-config
  [params]
  {:context (get params "context")
   :constants (let [const-params (get params "constants")
                    ks (map keyword (keys const-params))
                    vs (map edn/read-string (vals const-params))]
                (zipmap ks vs))
   :profs (let [prof-params (get params "profs")
                ks (map keyword (keys prof-params))
                vs (map #(-> (map (comp keyword s/trim)
                                  (s/split % #","))
                             set)
                        (vals prof-params))]
            (zipmap ks vs))
   :contrib (let [contrib-params (get params "contrib")
                  param-order (sort (map edn/read-string
                                         (keys contrib-params)))
                  sorted-contrib (map #(get contrib-params (str %))
                                      param-order)]
              (map (fn [contrib]
                     (zipmap (map keyword (keys contrib))
                             (map edn/read-string (vals contrib))))
                   sorted-contrib))
   :projects (let [sorted (sort-by #(edn/read-string (get % "rank"))
                                   (vals (get params "projects")))]
               (map param-to-project sorted))})

(defn add-params
  [config params]
  (if-let [change-param (get params "config-change")]
    (case change-param
      "Add Project" (let [projects (:projects config)
                          new-project {:name (get params "new-project")
                                       :effort {}}]
                      (assoc config
                             :projects
                             (conj projects new-project)))
      "Add Engineer" (let [profs (:profs config)
                           new-eng (keyword
                                    (get params "new-engineer"))]
                       (assoc config ;; probably can use update-in
                              :profs
                              (assoc profs new-eng #{:prof})))
      "Add Iteration" (let [engs (-> config :profs keys)
                            iters (-> config :contrib vec)
                            new-iter (zipmap engs (repeat 0))
                            new-iters (conj iters new-iter)]
                        (assoc config
                               :contrib
                               new-iters))
      config)
    config))


(defn with-removed
  "Takes `parts-indices` a map of `config` key->list of indices to be removed from
  the same key (keyword) in config at indices."
  [parts-indices config]
  (reduce (fn [config [part indices]]
            (let [kpart (keyword part)
                  before (get config kpart)
                  after (utils/remove-from before indices)]
              (assoc config kpart after)))
          config
          parts-indices))

(defn remove-params
  [config params]
  (if-let [change-param (get params "remove")]
    (with-removed
      (reduce (fn [res [part dparam]]
                (assoc res
                       part
                       (map edn/read-string (keys dparam))))
              {}
              change-param)
      config)
    config))

(defn update-params
  [config params]
  (remove-params (add-params config params)
                 params))

(defn available-profs
  [config]
  (apply st/union
         (-> config :profs vals)))

(defn available-engs
  [config]
  (let [from-profs (-> config :profs keys set)
        from-contrib (apply st/union (map (comp set keys)
                                          (-> config :contrib)))]
    (st/union from-contrib from-profs)))

(defn available-projects
  [config]
  (map :name (:projects config)))

(defn- numbered-unique
  ([base taken format-str]
   (utils/with-lookup [lookup taken]
     (if (get lookup base)
       (loop [num 1]
         (let [check (format format-str base num)]
           (if (get lookup check)
             (recur (inc num))
             check)))
       base)))
  ([base taken]
   (numbered-unique base taken "%s %d")))

(defn new-project-name
  [all-project-names]
  (numbered-unique "New project" all-project-names))

(defn new-eng-name
  [all-eng-names]
  (keyword (numbered-unique "engineer"
                            (map name all-eng-names)
                            "%s%d")))

(defn input
  [config-from-file config-name submit-action]
  (let [profs (available-profs config-from-file)
        engs (available-engs config-from-file)
        projects (available-projects config-from-file)]
    [:form {:action submit-action
            :method "GET"}
     [:fieldset [:legend "Add"]
      [:input {:type "input"
               :name "new-project"
               :value (new-project-name projects)}]
      [:input {:type "submit"
               :name "config-change"
               :value "Add Project"}]
      [:br]
      [:input {:type "input"
               :name "new-engineer"
               :value (new-eng-name engs)}]
      [:input {:type "submit"
               :name "config-change"
               :value "Add Engineer"}]
      [:br]
      [:input {:type "submit"
               :name "config-change"
               :value "Add Iteration"}]]
     [:button "Submit"]
     (t/input "input"
              "Context"
              "context"
              (:context config-from-file))
     (apply render-constants-inputs
            (map (:constants config-from-file)
                 [:sprints :unplanned :velocity]))
     (render-profs-inputs (:profs config-from-file))
     (render-contribs (:contrib config-from-file)
                      engs)
     (into [:fieldset.projects "Projects"]
           (map #(render-project (first %)
                                 (last %)
                                 profs)
                (utils/group-interleave (range)
                                        (:projects config-from-file))))
     [:button "Submit"]]))


