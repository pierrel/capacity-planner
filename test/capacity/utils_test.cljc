(ns capacity.utils-test
  (:require [capacity.utils :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true]))
  (:use [capacity.test-utils]))

(t/deftest sort-like
  (t/is (= [{:name :pierre :points 10}
            {:name :ana :points 20}
            {:name :lola :points 5}]
           (sut/sort-like [{:name :pierre}
                           {:name :ana}
                           {:name :lola}]
                          :name
                          [{:name :ana :points 20}
                           {:name :lola :points 5}
                           {:name :pierre :points 10}]))))

(t/deftest group-interleave
  (are-equal sut/group-interleave
             [[1 2 3]
              [:a :b :c]
              ["a" "b" "c"]]
             [[1 :a "a"]
              [2 :b "b"]
              [3 :c "c"]]))
