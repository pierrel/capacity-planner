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
                     (format "/%s/submit" config-name)))

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

     "/{config-name}/submit" ;; change this to be a POST
     (fn [{config-name :config-name}]
       (let [conf (config-view/params-to-config (-> request
                                                    nested-params-request
                                                    :params))]
         (try
           (-> conf config-view/output with-response)
           (catch RuntimeException e
             (with-response 422
               [:div
                [:div.error
                 [:p "Could not run config."]
                 [:p (.getMessage e)]]
                (input-view conf config-name)])))))
     (-> (response "Page not found")
         (status 404)))))

(def app (wrap-params routes))

