(ns web.handler
  (:use capacity.core
        ring.middleware.resource
        ring.middleware.nested-params
        ring.middleware.params
        ring.util.response
        ring.adapter.jetty)
  (:require [capacity.config :as config]
            [capacity.validation :as validation]
            [web.router :as router]
            [web.templates :as t]
            [web.views.config :as config-view]
            [web.views.output :as output-view]
            [hiccup.core :as h])
  (:import [capacity.core Eng Project]))

(def are-distinct? (partial apply distinct?))

(def config-pre-validations
  [{:message "Project names must be unique."
    :finder #(map :name (:projects %))
    :validator are-distinct?}])

(defn pre-validate
  [form]
  (apply (partial validation/validate form) config-pre-validations))

(defn with-response
  ([status-code resp]
   (-> (response (h/html (t/template resp)))
       (content-type "text/html")
       (status status-code)))
  ([resp]
   (with-response 200 resp)))

(defn input-view
  [config config-name]
  (config-view/input config
                     config-name
                     (format "/%s/submit" config-name)))

(defn submit-route
  [request params config-name]
  (let [nparams (-> request
                    nested-params-request
                    :params)
        conf (config-view/params-to-config nparams)]
    (if (or (get nparams "config-change")
            (get nparams "remove"))
      (with-response
        (try
          (input-view
           (pre-validate (config-view/update-params conf nparams))
           config-name)
          (catch RuntimeException e
            [:div.errors
             [:p "Could not add due to validation errors:"]
             [:p (ex-data e)]
             (into [:ul]
                   (mapv #(vector :li %) (:messages (ex-data e))))
             (input-view conf config-name)])))
      (try
        (-> conf output-view/summary with-response)
        (catch RuntimeException e
          (with-response 422
            [:div
             [:div.error
              [:p "Could not run config."]
              [:p (.getMessage e)]]
             (input-view conf config-name)]))))))

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

