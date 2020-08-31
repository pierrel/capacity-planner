(ns capacity.config-test
  (:require [capacity.config :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(def sample
  {:context "testing"
   :constants {:sprints 6 :unplanned 0.15 :velocity 5}
   :profs {:pierre #{:app :web}
           :selma #{:ios :android}}
   :contrib [{:pierre 1 :selma 0.5}
             {:pierre 0.5 :selma 2}]
   :projects [{:name "Proj1"
               :effort {:app 10
                        :ios 3}}
              {:name "Proj2"
               :effort {:app 2
                        :ios 4
                        :android 2}}]})

(t/deftest validate
  (t/is (map? (sut/validate sample)))
  (t/is (thrown-with-msg? RuntimeException
                          #"Validation errors"
                          (sut/validate (update-in sample
                                                   [:projects 0 :effort]
                                                   #(assoc % :meme 10))))))

(t/deftest read
  (t/is (= {:context "Test plan",
           :constants {:sprints 6, :unplanned 0.15, :velocity 5},
           :profs
           {:eric #{:app :web},
            :jen #{:web},
            :jordan #{:app :web},
            :cathy #{:app :data},
            :jeff #{:ios :app},
            :john #{:android :app},
            :jimmy #{:ios :app},
            :kent #{:app},
            :mary #{:android :app}},
           :contrib
           [{:eric 0.9,
             :jen 1,
             :jordan 1,
             :cathy 1,
             :jeff 0,
             :john 0.5,
             :jimmy 0.8,
             :kent 0,
             :mary 0.5}
            {:jeff 0,
             :kent 0,
             :cathy 1,
             :eric 0.9,
             :jen 1,
             :jordan 1,
             :john 0.5,
             :jimmy 0.8}
            {:jeff 1,
             :kent 1,
             :cathy 1,
             :eric 0.9,
             :jen 1,
             :jordan 1,
             :john 0.5,
             :jimmy 0.8}
            {:jeff 1,
             :kent 1,
             :cathy 1,
             :eric 0.9,
             :jen 1,
             :jordan 1,
             :john 1,
             :jimmy 0.8}],
           :projects
            '({:name "Dynamic FCap", :effort {:app 32}}
              {:name "A11y", :effort {:web 45}}
              {:name "Objectives", :effort {:web 4, :app 4}}
              {:name "Editability", :effort {:ios 3, :android 3, :app 32, :web 3}}
              {:name "Online Events Attribution", :effort {:app 20}}
              {:name "Test leads", :effort {:app 20}}
              {:name "TextViewModel migration",
               :effort {:app 5, :ios 2, :android 2, :web 3}}
              {:name "SSCS", :effort {:web 8, :app 30}}
              {:name "C2M", :effort {:ios 8, :android 8, :web 20, :app 40}}
              {:name "Deprecate Custom Senders",
               :effort {:app 20, :ios 8, :android 8, :web 8}}
              {:name "Sender Identity", :effort {:app 30, :web 20}}
              {:name "Human handoff",
               :effort {:app 40, :web 40, :android 10, :ios 10}})}
           (sut/read "test/resources/test-config.edn"))))
