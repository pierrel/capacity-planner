(ns capacity.validation-test
  (:require [capacity.validation :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true]))
  (:use [capacity.test-utils]))

(t/deftest validation-error
  (t/is (thrown? RuntimeException
                 (sut/validation-error [1 2]
                                       ["Must be better"]))))

(t/deftest valid-by?
  (are-equal sut/valid-by?
             [{:finder identity
               :validator (partial every? even?)}
              [1 2 3]]
             false

             [{:finder vals
               :validator (partial every? number?)}
              {:a 1 :b 2 :c 3}]
             true))

(t/deftest validation-messages
  (are-equal sut/validation-messages
             [{:a 1 :b 2 :c 3}
              [{:message "Values must all be numbers"
                :finder vals
                :validator (partial every? number?)}]]
             []

             [{:a 1 :b 2 :c 3}
              [{:message "Values must all be even numbers"
                :finder vals
                :validator (partial every? even?)}]]
             ["Values must all be even numbers"]))


(t/deftest validate
  (t/is (thrown? RuntimeException
                 (sut/validate {:a 1 :b 2 :c 3}
                               {:message "Values must all be even"
                                :finder vals
                                :validator (partial every? even?)})))
  (t/is (= {:a 1 :b 2 :c 3}
           (sut/validate {:a 1 :b 2 :c 3}
                         {:message "Values must all be numbers"
                          :finder vals
                          :validator (partial every? number?)}))))
