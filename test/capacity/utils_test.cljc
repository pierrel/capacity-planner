(ns capacity.utils-test
  (:require [capacity.utils :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

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
