(ns capacity.core-test
  (:require [capacity.core :as sut]
            [capacity.config :as config]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest update-teammate-capacity
  (let [capacity '({:name :pierre :capacity 10}
                   {:name :leo :capacity 3})]
    (t/is (= '({:name :pierre :capacity 4}
               {:name :leo :capacity 3})
             (sut/update-teammate-capacity capacity :pierre 4)))
    (t/is (= capacity
             (sut/update-teammate-capacity capacity :noone 10)))))

(t/deftest eng-work-on
  (t/is (= [{:capacity 5} 0]
           (sut/eng-work-on {:capacity 10} 5)))
  (t/is (= [{:capacity 0} 30]
           (sut/eng-work-on {:capacity 20} 50)))
  (t/is (= [{:capacity 0} 0]
           (sut/eng-work-on {:capacity 2} 2)))
  (t/is (= [{:capacity 0} 10]
           (sut/eng-work-on {:capacity 0} 10)))
  (t/is (= [{:capacity 10} 0]
           (sut/eng-work-on {:capacity 10} 0))))

(t/deftest team-work-on-tech
  (let [capacity '({:name :pierre :capacity 10 :profs #{:app}}
                   {:name :jonathan :capacity 15 :profs #{:app}}
                   {:name :leo :capacity 13 :profs #{:ios}})]
    (t/is (= ['({:name :pierre :capacity 10 :profs #{:app}}
                {:name :jonathan :capacity 15 :profs #{:app}}
                {:name :leo :capacity 3 :profs #{:ios}})
              0]
             (sut/team-work-on-tech capacity :ios 10)))
    (t/is (= ['({:name :pierre :capacity 10 :profs #{:app}}
                {:name :jonathan :capacity 15 :profs #{:app}}
                {:name :leo :capacity 0 :profs #{:ios}})
              3]
             (sut/team-work-on-tech capacity :ios 16)))
    (t/is (= ['({:name :pierre :capacity 0 :profs #{:app}}
                {:name :jonathan :capacity 3 :profs #{:app}}
                {:name :leo :capacity 13 :profs #{:ios}})
              0]
             (sut/team-work-on-tech capacity :app 22)))))

(t/deftest work-on-project
  (let [capacity '({:name :pierre :capacity 10 :profs #{:app}}
                   {:name :jonathan :capacity 15 :profs #{:app}}
                   {:name :leo :capacity 13 :profs #{:ios}})
        project {:name "something" :effort {:ios 10 :app 20}}]
    (t/is (= [{:name "something" :effort {:ios 0 :app 0}}
              '({:name :pierre :capacity 0 :profs #{:app}}
                {:name :jonathan :capacity 5 :profs #{:app}}
                {:name :leo :capacity 3 :profs #{:ios}})]
             (sut/work-on-project project capacity)))
    (t/is (= [{:name "something" :effort {:ios 0 :app 0}}
              '({:name :ernesto :capacity 10 :profs #{:app :web}}
                {:name :pierre :capacity 0 :profs #{:app}}
                {:name :jonathan :capacity 5 :profs #{:app}}
                {:name :leo :capacity 3 :profs #{:ios}})]
             (sut/work-on-project project
                                  (conj capacity
                                        {:name :ernesto
                                         :capacity 10
                                         :profs #{:app :web}}))))
    (t/is (= [{:name "something else" :effort {:web 10 :app 5}}
              '({:name :pierre, :capacity 0, :profs #{:app}}
                {:name :jonathan, :capacity 0, :profs #{:app}}
                {:name :leo, :capacity 3, :profs #{:ios}} )]
             (sut/work-on-project
              {:name "something else" :effort {:web 10 :app 10}}
              '({:name :pierre, :capacity 0, :profs #{ :app } }
                { :name :jonathan, :capacity 5, :profs #{ :app } }
                { :name :leo, :capacity 3, :profs #{ :ios } } ))))))

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
             (sut/work-on projects capacity)))))

(t/deftest work-on-long
  (let [config (config/read "test/resources/test-config.edn")
        projects(:projects config)
        const (:constants config)]
    (t/is (= [{:completed '("Dynamic FCap" "Online Events Attribution"),
            :progressed
            '({:name "A11y", :effort {:web 3.0500000000000007}}
             {:name "Objectives", :effort {:web 4, :app 0}}
             {:name "Editability", :effort {:ios 0, :android 0, :app 0, :web 3}}
             {:name "Test leads", :effort {:app 10.599999999999998}}),
            :remaining-team ()}
           {:completed
            '("A11y"
             "Objectives"
             "Editability"
             "Test leads"
             "TextViewModel migration"
             "SSCS"),
            :progressed
            '({:name "C2M",
              :effort {:ios 0, :android 0, :web 0, :app 14.049999999999997}}),
            :remaining-team ()}
           {:completed '("C2M" "Deprecate Custom Senders" "Sender Identity"),
            :progressed
            '({:name "Human handoff",
              :effort {:app 0, :web 29.599999999999998, :android 5.25, :ios 0}}),
            :remaining-team
            '({:name :jimmy, :profs #{:ios :app}, :capacity 10.400000000000002})}
           {:completed '("Human handoff"),
            :progressed (),
            :remaining-team
            '({:name :jeff, :profs #{:ios :app}, :capacity 25.5}
             {:name :kent, :profs #{:app}, :capacity 25.5}
             {:name :cathy, :profs #{:app :data}, :capacity 25.5}
             {:name :eric, :profs #{:app :web}, :capacity 18.85}
             {:name :jordan, :profs #{:app :web}, :capacity 25.5}
             {:name :john, :profs #{:android :app}, :capacity 20.25}
             {:name :jimmy, :profs #{:ios :app}, :capacity 20.400000000000002})}]
             (sut/work-on-long projects
                               (:contrib config)
                               const
                               (:profs config))))))

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

(t/deftest has-effort?
  (t/is (= true
           (sut/has-effort? {:effort {:app 10 :ios 0}})))
  (t/is (= nil
           (sut/has-effort? {:effort {:app 0 :web 0}}))))

(t/deftest update-effort
  (t/is (= {:effort {:app 25
                     :ios 15}
            :name "whatever"}
           (sut/update-effort {:effort {:app 10
                                         :ios 15}
                                :name "whatever"}
                               {:app 25}))))
