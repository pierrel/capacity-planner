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
  "Take a coll of colls and interleaves the elements:

  [[a b c] [1 2 3]] -> [[a 1] [b 2] [c 3]]
  [[a b c d] [1 2 3] [f g h]] -> [[a 1 f] [b 2 g] [c 3 h]]"
  [& colls]
  (partition (count colls)
             (apply interleave colls)))

(defn insert
  "Returns a new seq where `x` is inserted into the `n` position of `coll`"
  [x n coll]
  (let [[before after] (split-at n coll)]
    (concat before (cons x after))))

(defn- update-to-nil
  "Replaces all elements in `coll` at `indices` with nil."
  [coll indices]
  (let [lookup (zipmap indices (repeat true))]
    (map-indexed (fn [i el]
                   (if (get lookup i false)
                     nil
                     el))
                 coll)))

(defn remove-from
  "Removes elements at `indices` from `coll`.

  Assumes that none of the elements are nil."
  [coll indices]
  (remove nil? (update-to-nil coll indices)))

(defn- to-lookup
  [coll]
  (zipmap coll (repeat true)))

(defmacro with-lookup
  "Evaluates `forms` in the lexical context of `bindings` in which init-exprs are converted to maps.

  Similar to `let`, but converts colls to a map where each element is associated
  to true for efficient lookup."
  [bindings & forms]
  (let [parted (partition 2 bindings)
        bsym (map first parted)
        bcoll (mapv #(list to-lookup %) (map last parted))
        reformed (vec (interleave bsym bcoll))]
    `(let ~reformed
       ~@forms)))
