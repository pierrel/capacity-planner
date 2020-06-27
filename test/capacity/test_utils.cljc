(ns capacity.test-utils
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(defmacro is-equal
  [f input output]
  `(t/is (= ~output
            (~f ~@input))))

(defmacro are-equal
  [f & in-out-pairs]
  (let [paired (partition 2 in-out-pairs)
        calls (map #(list 'is-equal f (first %) (last %)) paired)]
    `(do ~@calls)))
