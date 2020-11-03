(ns capacity.core-test
  (:require [capacity.core :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true]))
  (:use [capacity.test-utils])
  (:import [capacity.core Eng Project Change]))

(t/deftest merge-cp-solution
  (are-equal sut/merge-cp-solution
             ;; Golden path
             [{:pierre {:app 3 :web 4}
               :ana {:ios 2 :app 5}}
              (sut/Project. :one {:app 10 :web 10 :ios 10})
              [(sut/Eng. :pierre #{:app :web} 10)
               (sut/Eng. :ana #{:ios :app} 10)]]
             [(sut/Project. :one {:app 2 :web 6 :ios 8})
              [(sut/Eng. :pierre #{:app :web} 3)
               (sut/Eng. :ana #{:ios :app} 3)]]

             ;; One eng incapable
             [{:pierre {:app 3 :web 4}}
              (sut/Project. :one {:app 10 :web 10 :ios 10})
              [(sut/Eng. :pierre #{:app :web} 10)
               (sut/Eng. :ana #{:android} 10)]]
             [(sut/Project. :one {:app 7 :web 6 :ios 10})
              [(sut/Eng. :pierre #{:app :web} 3)
               (sut/Eng. :ana #{:android} 10)]]

             ;; No eng capable
             [{}
              (sut/Project. :one {:app 10 :web 10 :ios 10})
              [(sut/Eng. :pierre #{:android} 10)
               (sut/Eng. :ana #{:android} 10)]]
             [(sut/Project. :one {:app 10 :web 10 :ios 10})
              [(sut/Eng. :pierre #{:android} 10)
               (sut/Eng. :ana #{:android} 10)]]))

(t/deftest work-out
  (let [proj (sut/Project. :something {:app 10 :web 10 :ios 5})
        jan (sut/Eng. :jan #{:web :app} 5)
        paul (sut/Eng. :paul #{:web} 5)
        jean (sut/Eng. :jean #{:ios} 5)
        brolly (sut/Eng. :brolly #{:app :ios :web} 16)]
    (are-equal sut/work-out
                      [proj [jan]]
                      [(assoc proj :effort {:app 5.0 :web 10.0 :ios 5})
                       [(assoc jan :capacity 0.0)]]

                      [proj [jan paul]]
                      [(assoc proj :effort {:app 5.0 :web 5.0 :ios 5})
                       [(assoc jan :capacity 0.0)
                        (assoc paul :capacity 0.0)]]

                      [proj [jan paul jean]]
                      [(assoc proj :effort {:app 5.0 :web 5.0 :ios 0.0})
                       [(assoc jan :capacity 0.0)
                        (assoc paul :capacity 0.0)
                        (assoc jean :capacity 0.0)]]

                      [proj [jan paul jean brolly]]
                      [(assoc proj :effort {:app 0.0 :web 0.0 :ios 0.0})
                       [(assoc jan :capacity 0.0)
                        (assoc paul :capacity 0.0)
                        (assoc jean :capacity 0.0)
                        (assoc brolly :capacity 6.0)]])))

(t/deftest work-backlog
  (let [frontback (sut/Project. :frontback {:app 10 :web 10 :ios 5})
        justback (sut/Project. :justback {:app 3})
        jan (sut/Eng. :jan #{:web :app} 5)
        paul (sut/Eng. :paul #{:web} 5)
        jean (sut/Eng. :jean #{:ios} 5)
        brolly (sut/Eng. :brolly #{:app :ios :web} 16)]
    (are-equal sut/work-backlog
                      [[frontback] [jan paul jean]]
                      [[(assoc frontback :effort {:app 5.0
                                                  :web 5.0
                                                  :ios 0.0})]
                       [(assoc jan :capacity 0.0)
                        (assoc paul :capacity 0.0)
                        (assoc jean :capacity 0.0)]]

                      [[frontback justback] [jan paul jean]]
                      [[(assoc frontback :effort {:app 5.0 :web 5.0 :ios 0.0})
                        (assoc justback :effort {:app 3.0})]
                       [(assoc jan :capacity 0.0)
                        (assoc paul :capacity 0.0)
                        (assoc jean :capacity 0.0)]]

                      [[frontback justback] [jan paul jean brolly]]
                      [[(assoc frontback :effort {:app 0.0 :web 0.0 :ios 0.0})
                        (assoc justback :effort {:app 0.0})]
                       [(assoc jan :capacity 0.0)
                        (assoc paul :capacity 0.0)
                        (assoc jean :capacity 0.0)
                        (assoc brolly :capacity 3.0)]])))

;; Finite
(t/deftest diff
  (are-equal sut/diff
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
(t/deftest exhausted?
  (are-equal sut/exhausted?
             [(Project. :full {:app 10 :web 3})]
             false

             [(Project. :half {:app 10 :web 0})]
             false

             [(Project. :done {:app 0 :web 0})]
             true

             [(Eng. :ready #{} 4)]
             false

             [(Eng. :done #{} 0)]
             true))

(sut/summarize-all [(Project. :med {:app 10 :web 10})
                    (Project. :large {:app 15 :web 20})]
                   [(Project. :med {:app 5 :web 5})
                    (Project. :large {:app 0 :web 3})]
                   [(Project. :med {:app 10 :web 10})
                    (Project. :large {:app 15 :web 20})])

(t/deftest summarize-all
  (are-equal sut/summarize-all
                    [[(Project. :med {:app 10 :web 10})
                      (Project. :large {:app 15 :web 20})]
                     [(Project. :med {:app 5 :web 5})
                      (Project. :large {:app 0 :web 3})]
                     [(Project. :med {:app 10 :web 10})
                      (Project. :large {:app 15 :web 20})]]
                    [(Change. :med
                              true
                              {:app -5 :web -5}
                              1/2)
                     (Change. :large
                              true
                              {:app -15 :web -17}
                              32/35)]

                    [[(Eng. :josh #{} 10)
                      (Eng. :jess #{} 5)]
                     [(Eng. :josh #{} 0)
                      (Eng. :jess #{} 3)]
                     [(Eng. :josh #{} 10)
                      (Eng. :jess #{} 5)]]
                    [(Change. :josh
                              true
                              {:capacity -10}
                              1)
                     (Change. :jess
                              true
                              {:capacity -2}
                              2/5)]))

(t/deftest work-backlog-iter
  (are-equal sut/work-backlog-iter
             [[(Project. :med {:app 10 :web 10})
               (Project. :large {:app 15 :web 20})]
              [[(Eng. :pierre #{:app :web} 10)
                (Eng. :jan #{:web} 5)]
               [(Eng. :pierre #{:app :web} 10)
                (Eng. :jan #{:web} 5)]]]

             [[(Project. :med {:app 0.0 :web 0.0})
               (Project. :large {:app 5.0 :web 20.0})]
              [[(Project. :med {:app 10 :web 10})
                (Project. :large {:app 15 :web 20})]
               [(Project. :med {:app 0.0 :web 5.0})
                (Project. :large {:app 15.0 :web 20.0})]]
              [(list (Change. :med
                              true
                              {:app -10.0 :web -5.0}
                              0.75)
                     (Change. :large
                              true
                              {:app 0.0 :web 0.0}
                              0.0))
               (list (Change. :med
                              true
                              {:app 0.0 :web -5.0}
                              1.0)
                     (Change. :large
                              true
                              {:app -10.0 :web 0.0}
                              0.2857142857142857))]
              [(list (Change. :pierre
                              true
                              {:capacity -10.0}
                              1.0)
                     (Change. :jan
                              true
                              {:capacity -5.0}
                              1.0))
               (list (Change. :pierre
                              true
                              {:capacity -10.0}
                              1.0)
                     (Change. :jan
                              true
                              {:capacity -5.0}
                              1.0))]]))
