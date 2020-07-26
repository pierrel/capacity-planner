(ns web.router-test
  (:require [web.router :as sut]
            [clojure.test :as t])
  (:use [capacity.test-utils]))

(t/deftest routes
  (let [pairs ["/" "slash"
               "/something" "slash something"
               "/else" (fn [] "hahah")
               "defaulting here"]
        on-uri (fn [uri]
                 (apply (partial sut/routes uri) pairs))]
    (are-equal on-uri
               ["/"] "slash"
               ["/else"] "hahah"
               ["/nothing-here"] "defaulting here")))
