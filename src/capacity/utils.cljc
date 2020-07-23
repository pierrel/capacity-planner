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

(defn insert
  "Returns a new seq where `x` is inserted into the `n` position of `coll`"
  [x n coll]
  (let [[before after] (split-at n coll)]
    (concat before (cons x after))))
