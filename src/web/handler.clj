(ns web.handler
  (:use capacity.core
        ring.middleware.resource
        ring.middleware.nested-params
        ring.middleware.params
        ring.util.response
        ring.adapter.jetty)
  (:require [capacity.config :as config]
            [web.router :as router]
            [web.templates :as t]
            [web.views.config :as config-view]
            [hiccup.core :as h])
  (:import [capacity.core Eng Project]))

(defn input-view
  [config config-name]
  (config-view/input config
                     config-name
                     (format "/%s/submit" config-name)
                     (format "/%s/add-project" config-name)))

(defn submit-route
  [request params config-name]
  (let [conf (config-view/params-to-config (-> request
                                               nested-params-request
                                               :params))]
    (if-let [change-param (get params "config-change")]
      (with-response
        (input-view
         (case change-param
           "Add Project" (let [projects (:projects conf)
                               new-project {:name (get params "new-project")
                                            :effort {}}]
                           (println (str new-project))
                           (assoc conf
                                  :projects
                                  (conj projects new-project)))
           "Add Engineer" (let [profs (:profs conf)
                                new-eng (keyword
                                         (get params "new-engineer"))]
                            (assoc conf ;; probably can use update-in
                                   :profs
                                   (assoc profs new-eng #{}))))
         config-name))
      (try
        (-> conf config-view/output with-response)
        (catch RuntimeException e
          (with-response 422
            [:div
             [:div.error
              [:p "Could not run config."]
              [:p (.getMessage e)]]
             (input-view conf config-name)]))))))

(defn with-response
  ([status-code resp]
   (-> (response (h/html (t/template resp)))
       (content-type "text/html")
       (status status-code)))
  ([resp]
   (with-response 200 resp)))

(defn routes [request]
  (let [{uri    :uri
         params :params} request]
    (router/routes
     uri
     "/{config-name}"
     (fn [{config-name :config-name}]
       (let [config (config/read (str config-name ".edn"))]
         (with-response
           (input-view config
                       config-name))))

     "/{config-name}/submit"
     (fn [{config-name :config-name}]
       (submit-route request params config-name))

     (-> (response "Page not found")
         (status 404)))))

(def app (wrap-resource (wrap-params routes) "public"))

