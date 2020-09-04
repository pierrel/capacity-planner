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
              [3 :c "c"]]

             [(range)
              [:a :b :c :d]]
             [[0 :a]
              [1 :b]
              [2 :c]
              [3 :d]]))

(t/deftest insert
  (are-equal sut/insert
             [1 2 '(1 2 3 4)]
             '(1 2 1 3 4)

             ['(3 4 5) 2 '(1 2 6 7)]
             '(1 2 (3 4 5) 6 7)))

(t/deftest remove-from
  (are-equal sut/remove-from
             [[1 2 3 4] [0 1]]
             '(3 4)

             ['() '()]
             '()

             ['(0 1 2) [2]]
             '(0 1)))

(t/deftest with-lookup
  (sut/with-lookup [l [:a :b :c]]
    (t/is (= true
             (get l :a)))
    (t/is (= false
             (get l :z false)))))
