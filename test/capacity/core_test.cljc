(ns capacity.core-test
  (:require [capacity.core :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest update-capacity
  (let [capacity '({:name :pierre :capacity 10}
                   {:name :leo :capacity 3})]
    (t/is (= '({:name :pierre :capacity 4}
               {:name :leo :capacity 3})
             (sut/update-capacity capacity :pierre 4)))
    (t/is (= capacity
             (sut/update-capacity capacity :noone 10)))))

(t/deftest work-on-tech
  (let [capacity '({:name :pierre :capacity 10 :profs #{:app}}
                   {:name :jonathan :capacity 15 :profs #{:app}}
                   {:name :leo :capacity 13 :profs #{:ios}})]
    (t/is (= [0 '({:name :pierre :capacity 10 :profs #{:app}}
                  {:name :jonathan :capacity 15 :profs #{:app}}
                  {:name :leo :capacity 3 :profs #{:ios}})]
             (sut/work-on-tech capacity :ios 10)))
    (t/is (= [3 '({:name :pierre :capacity 10 :profs #{:app}}
                  {:name :jonathan :capacity 15 :profs #{:app}}
                  {:name :leo :capacity 0 :profs #{:ios}})]
             (sut/work-on-tech capacity :ios 16)))
    (t/is (= [0 '({:name :pierre :capacity 0 :profs #{:app}}
                  {:name :jonathan :capacity 3 :profs #{:app}}
                  {:name :leo :capacity 13 :profs #{:ios}})]
             (sut/work-on-tech capacity :app 22)))))

(t/deftest work-on-project
  (let [capacity '({:name :pierre :capacity 10 :profs #{:app}}
                   {:name :jonathan :capacity 15 :profs #{:app}}
                   {:name :leo :capacity 13 :profs #{:ios}})
        project {:name "something" :effort {:ios 10 :app 20}}]
    (t/is (= [{:name "something" :effort {:ios 0 :app 0}}
              '({:name :pierre :capacity 0 :profs #{:app}}
                {:name :jonathan :capacity 5 :profs #{:app}}
                {:name :leo :capacity 3 :profs #{:ios}})]
             (sut/work-on-project capacity project)))
    (t/is (= [{:name "something" :effort {:ios 0 :app 0}}
              '({:name :ernesto :capacity 10 :profs #{:app :web}}
                {:name :pierre :capacity 0 :profs #{:app}}
                {:name :jonathan :capacity 5 :profs #{:app}}
                {:name :leo :capacity 3 :profs #{:ios}})]
             (sut/work-on-project (conj capacity
                                        {:name :ernesto
                                         :capacity 10
                                         :profs #{:app :web}})
                                  project)))
    (t/is (= [{:name "something else" :effort {:web 10 :app 5}}
              '({:name :pierre, :capacity 0, :profs #{:app}}
                {:name :jonathan, :capacity 0, :profs #{:app}}
                {:name :leo, :capacity 3, :profs #{:ios}} )]
             (sut/work-on-project
              '({:name :pierre, :capacity 0, :profs #{ :app } }
                { :name :jonathan, :capacity 5, :profs #{ :app } }
                { :name :leo, :capacity 3, :profs #{ :ios } } )
              {:name "something else" :effort {:web 10 :app 10}})))))

(t/deftest work-on
  (let [capacity '({:name :pierre :capacity 10 :profs #{:app}}
                   {:name :jonathan :capacity 15 :profs #{:app}}
                   {:name :leo :capacity 13 :profs #{:ios}})
        projects '({:name "something" :effort {:ios 10 :app 20}}
                    {:name "something else" :effort {:web 10 :app 10}})]
    (t/is (= ['({:name "something", :effort {:ios 0, :app 0}}
                {:name "something else", :effort {:web 10, :app 5}})
              '({:name :pierre, :capacity 0, :profs #{:app}}
                {:name :jonathan, :capacity 0, :profs #{:app}}
                {:name :leo, :capacity 3, :profs #{:ios}})]
             (sut/work-on capacity projects)))))

(t/deftest has-prof-avaialble?
  (t/is (= false
           (sut/has-prof-available? {:capacity 0 :profs #{:app}}
                                    :app)))
  (t/is (= false
           (sut/has-prof-available? {:capacity 10 :profs #{:app}}
                                    :ios)))
  (t/is (= true
           (sut/has-prof-available? {:capacity 1 :profs #{:app}}
                                    :app))))


(t/deftest update-capacities
  (t/is (= '({:name :pierre :capacity 10}
             {:name :leo :capacity 5})
           (sut/update-capacities '({:name :pierre :capacity 5}
                                    {:name :leo :capacity 3})
                                  '({:name :leo :capacity 5}
                                    {:name :pierre :capacity 10})))))

(t/deftest has-effort?
  (t/is (= true
           (sut/has-effort? {:effort {:app 10 :ios 0}})))
  (t/is (= nil
           (sut/has-effort? {:effort {:app 0 :web 0}}))))

(t/deftest update-project
  (t/is (= {:effort {:app 25
                     :ios 15}}
           (sut/update-project {:effort {:app 10
                                         :ios 15}}
                               :app
                               25))))

(t/deftest update-projects
  (t/is (= '({:name "something" :effort {:app 2 :ios 5}}
             {:name "else" :effort {:app 25 :ios 10}})
           (sut/update-projects '({:name "something" :effort {:app 12
                                                              :ios 15}}
                                  {:name "else" :effort {:app 25
                                                         :ios 10}})
                                {:name "something" :effort {:app 2
                                                            :ios 5}}))))
