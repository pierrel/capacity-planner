(ns capacity.utils)

(defn replace-with
  "Replaces any element in coll where (f e coll-element) is true."
  [f coll e]
  (map #(if (f e %)
          e
          %)
       coll))
