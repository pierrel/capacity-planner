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


(defn with-response [resp]
  (-> (response (h/html (t/template resp)))
      (content-type "text/html")
      (status 200)))

(defn routes [request]
  (let [{uri    :uri
         params :params} request]
    (router/routes
     uri
     "/{config-name}"
     (fn [{config-name :config-name}]
       (with-response
         (config-view/output config-name)))

     "/input/{config-name}"
     (fn [{config-name :config-name}]
       (let [config (config/read (str config-name ".edn"))]
         (with-response
           (config-view/input config config-name))))

     "/input/{config-name}/submit" ;; change this to be a POST
     (fn [{config-name :config-name}]
       (let [conf (params-to-config (-> request
                                        nested-params-request
                                        :params))]
         (with-response
           (into [:div]
                 (try
                   (config-view/output conf)
                   (catch RuntimeException e
                     [[:div.error
                       [:p "Could not save config."]
                       [:p (.getMessage e)]]
                      (config-view/input conf config-name)]))))))
     (-> (response "Page not found")
         (status 404)))))

(def app (wrap-params routes))

