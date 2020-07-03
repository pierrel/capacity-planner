(ns capacity.test-utils
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(defmacro are-equal
  [f & in-out-pairs]
  `(t/are [input expected] (= expected (apply ~f input))
     ~@in-out-pairs))
