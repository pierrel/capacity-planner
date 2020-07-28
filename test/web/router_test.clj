(ns web.router-test
  (:require [web.router :as sut]
            [clojure.test :as t])
  (:use [capacity.test-utils]))

(t/deftest routes
  (let [simple-pairs ["/" "slash"
                      "/something" "slash something"
                      "/else" (fn [p] "hahah")
                      "defaulting here"]
        param-pairs ["/" "slash"
                     "/{something}" (fn [p]
                                      (str "something-"
                                           (:something p)))
                     "/{something}/else/{more}-123" (fn [p]
                                                      (str "something-"
                                                           (:something p)
                                                           "more-"
                                                           (:more p)))
                     "default"]
        routes-on-uri (fn [route-pairs uri]
                        (apply (partial sut/routes uri) route-pairs))]
    (are-equal (partial routes-on-uri simple-pairs)
               ["/"] "slash"
               ["/else"] "hahah"
               ["/nothing-here"] "defaulting here")
    (are-equal (partial routes-on-uri param-pairs)
               ["/"] "slash"
               ["/me"] "something-me"
               ["/haha/else/hehe-123"] "something-hahamore-hehe"
               ["/no/nothing/here"] "default")))
