(ns web.handler
  (:use ring.middleware.resource
        ring.util.response
        ring.adapter.jetty)
  (:require [capacity.runner :as runner]))

(defn with-response [resp]
  (-> (response resp)
      (content-type "text/html")
      (status 200)))

(defn routes [{uri :uri}]
  (case uri
    "/" (with-response (runner/run-and-report "config-fy21.edn"))
    (-> (response "Page not found")
        (status 404))))

(def app routes)

