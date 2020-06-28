(ns capacity.utils)

(defn replace-with
  "Replaces any element in coll where (f e coll-element) is true."
  [f coll e]
  (map #(if (f e %)
          e
          %)
       coll))

(defn sort-like
  "Returns `unordered` sorted like `example` by `unique-key`"
  [example unique-key unordered]
  (let [lookup (reduce #(assoc %1
                               (-> %2 last unique-key)
                               (first %2))
                       {}
                       (partition 2 (interleave (range) example)))]
    (sort-by (comp lookup unique-key)
             unordered)))

(defn group-interleave
  [& colls]
  (partition (count colls)
             (apply interleave colls)))
