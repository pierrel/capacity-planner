(ns capacity.cp-test
  (:require [capacity.cp :as sut]
            [clojure.test :as t])
  (:use [capacity.test-utils]))

(t/deftest solve
  (are-equal sut/solve
             ;; No available profs
             [{:app 10 :web 10}
              [{:name :pierre :capacity 10 :profs #{:ios}}
               {:name :ana :capacity 10 :profs #{:android}}]]
             {}

             ;; Fully capable
             [{:app 10 :web 10}
              [{:name :pierre :capacity 10 :profs #{:app}}
               {:name :ana :capacity 10 :profs #{:web}}]]
             {:pierre {:app 10.0}
              :ana {:web 10.0}}

             ;; One incapable
             [{:app 10 :web 10}
              [{:name :pierre :capacity 10 :profs #{:app}}
               {:name :ana :capacity 10 :profs #{:android}}]]
             {:pierre {:app 10.0}}))
