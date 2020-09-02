(ns web.views.config-test
  (:require [web.views.config :as sut]
            [clojure.test :as t])
  (:use [capacity.test-utils]))

(t/deftest remove-params
  (let [config {:projects [1 2 3]
                :contrib '({:a 1}
                           {:b 2}
                           {:c 3})}
        params {"remove" {"projects" {"0" "Remove"
                                      "2" "Remove"}
                          "contrib" {"1" "Remove"}}}]
    (t/is (= {:projects '(2)
              :contrib '({:a 1}
                         {:c 3})}
             (sut/remove-params config params)))))
