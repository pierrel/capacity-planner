(ns capacity.modeling-test
  (:require [capacity.modeling :as sut]
            [capacity.test-utils :as tutils]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true]))
  (:import [capacity.modeling Eng Project]))


(t/deftest capacity-to-points
  (is-equal sut/capacity-to-points [10 20] [0 10])
  (is-equal sut/capacity-to-points [20 5] [15 0])
  (is-equal sut/capacity-to-points [10 10] [0 0])
  (is-equal sut/capacity-to-points [5 0] [5 0])
  (is-equal sut/capacity-to-points [0 2] [0 2]))

(t/deftest capacity-to-points
  (tutils/are-equal sut/capacity-to-points
                    [10 20] [0 10]
                    [20 5] [15 0]
                    [10 10] [0 0]
                    [5 0] [5 0]
                    [0 2] [0 2]))

(t/deftest capacity-to-effort
  (t/is (= [2 {:app 0 :web 0}]
           (sut/capacity-to-effort 18
                                   {:app 10 :web 6})))
  (t/is (= [0 {:app 0 :web 4}]
           (sut/capacity-to-effort 20
                                   {:app 10 :web 14})))
  (t/is (= [0 {:app 10 :web 5}]
           (sut/capacity-to-effort 0
                                   {:app 10 :web 5}))))

(t/deftest doable
  (tutils/are-equal sut/doable
                    [(sut/Eng. :john #{:app :web} 2)
                     {:app 10 :web 5}]
                    {:app 10 :web 5}

                    [(sut/Eng. :peter #{:ios} 3)
                     {:app 10 :web 5}]
                    {}

                    [(sut/Eng. :cathy #{:one :two} 200)
                     {:one 1 :two 2 :three 3}]
                    {:one 1 :two 2}))

(t/deftest work-on
  (tutils/are-equal sut/work-on
                    [(sut/Eng. :pierre #{:web :ios} 14)
                     (sut/Project. :simple {:web 10 :app 20 :ios 4})]
                    [(sut/Eng. :pierre #{:web :ios} 0)
                     (sut/Project. :simple {:web 0 :app 20 :ios 0})]

                    [(sut/Eng. :pierre #{:web :ios} 14)
                     (sut/Project. :simple {:app 20})]
                    [(sut/Eng. :pierre #{:web :ios} 14)
                     (Project. :simple {:app 20})]

                    [(sut/Eng. :pierre #{:web :ios} 15)
                     (Project. :simple {:web 50 :app 20 :ios 4})]
                    [(sut/Eng. :pierre #{:web :ios} 0)
                     (Project. :simple {:web 35 :app 20 :ios 4})]

                    [(sut/Eng. :pierre #{:web :ios} 14)
                     (Project. :simple {:web 10 :ios 4})]
                    [(sut/Eng. :pierre #{:web :ios} 0)
                     (Project. :simple {:web 0 :ios 0})]))

(t/deftest work-out
  (let [proj (sut/Project. :something {:app 10 :web 10 :ios 5})
        jan (sut/Eng. :jan #{:web :app} 5)
        paul (sut/Eng. :paul #{:web} 5)
        jean (sut/Eng. :jean #{:ios} 5)
        brolly (sut/Eng. :brolly #{:app :ios :web} 16)]
    (tutils/are-equal sut/work-out
                      [proj [jan]]
                      [(assoc proj :effort {:app 5 :web 10 :ios 5})
                       [(assoc jan :capacity 0)]]

                      [proj [jan paul]]
                      [(assoc proj :effort {:app 5 :web 5 :ios 5})
                       [(assoc jan :capacity 0)
                        (assoc paul :capacity 0)]]

                      [proj [jan paul jean]]
                      [(assoc proj :effort {:app 5 :web 5 :ios 0})
                       [(assoc jan :capacity 0)
                        (assoc paul :capacity 0)
                        (assoc jean :capacity 0)]]

                      [proj [jan paul jean brolly]]
                      [(assoc proj :effort {:app 0 :web 0 :ios 0})
                       [(assoc jan :capacity 0)
                        (assoc paul :capacity 0)
                        (assoc jean :capacity 0)
                        (assoc brolly :capacity 6)]])))

(t/deftest work-backlog
  (let [frontback (sut/Project. :frontback {:app 10 :web 10 :ios 5})
        justback (sut/Project. :justback {:app 3})
        jan (sut/Eng. :jan #{:web :app} 5)
        paul (sut/Eng. :paul #{:web} 5)
        jean (sut/Eng. :jean #{:ios} 5)
        brolly (sut/Eng. :brolly #{:app :ios :web} 16)]
    (tutils/are-equal sut/work-backlog
                      [[frontback] [jan paul jean]]
                      [[(assoc frontback :effort {:app 5 :web 5 :ios 0})]
                       [(assoc jan :capacity 0)
                        (assoc paul :capacity 0)
                        (assoc jean :capacity 0)]]

                      [[frontback justback] [jan paul jean]]
                      [[(assoc frontback :effort {:app 5 :web 5 :ios 0})
                        justback]
                       [(assoc jan :capacity 0)
                        (assoc paul :capacity 0)
                        (assoc jean :capacity 0)]]

                      [[frontback justback] [jan paul jean brolly]]
                      [[(assoc frontback :effort {:app 0 :web 0 :ios 0})
                        (assoc justback :effort {:app 0})]
                       [(assoc jan :capacity 0)
                        (assoc paul :capacity 0)
                        (assoc jean :capacity 0)
                        (assoc brolly :capacity 3)]])))

;; Finite
(t/deftest exhausted?
  (tutils/are-equal sut/exhausted?
                    [(Project. :some {:app 0 :web 10})]
                    false

                    [(Project. :ah {:me 0 :web 0})]
                    true

                    [(Eng. :pierre #{:one :two} 10)]
                    false

                    [(Eng. :pierre #{:two :one} 0)]
                    true))
(t/deftest diff
  (tutils/are-equal sut/diff
                    ;; Project
                    [(Project. :one {:app 10 :web 3})
                     (Project. :one {:app 5 :web 0})]
                    {:app -5 :web -3}

                    [(Project. :one {:app 10 :web 3})
                     (Project. :one {:app 10 :web 3})]
                    {:app 0 :web 0}

                    [(Project. :one {:app 10 :web 3})
                     (Project. :one {:app 10 :web 4 :ios 5})]
                    {:app 0 :web 1 :ios 5}

                    ;; Eng
                    [(Eng. :pierre #{} 5)
                     (Eng. :pierre #{} 0)]
                    {:capacity -5}

                    [(Eng. :pierre #{} 3)
                     (Eng. :pierre #{} 3)]
                    {:capacity 0}

                    [(Eng. :pierre #{} 4)
                     (Eng. :pierre #{} 10)]
                    {:capacity 6}))
