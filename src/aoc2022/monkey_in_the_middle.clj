(ns aoc2022.monkey-in-the-middle)

(def sample-monkeys
     (sorted-map
       0 {:items [79 98] :op #(* % 19) :test [23 2 3] :inspections 0}
       1 {:items [54 65 75 74] :op #(+ % 6) :test [19 2 0] :inspections 0}
       2 {:items [79 60 97] :op #(* % %) :test [13 1 3] :inspections 0}
       3 {:items [74] :op #(+ % 3) :test [17 0 1] :inspections 0}))

(def actual-monkeys
     (sorted-map
       0 {:items [50 70 54 83 52 78] :op #(* % 3) :test [11 2 7] :inspections 0}
       1 {:items [71 52 58 60 71] :op #(* % %) :test [7 0 2] :inspections 0}
       2 {:items [66 56 56 94 60 86 73] :op #(+ % 1) :test [3 7 5] :inspections 0}
       3 {:items [83 99] :op #(+ % 8) :test [5 6 4] :inspections 0}
       4 {:items [98 98 79] :op #(+ % 3) :test [17 1 0] :inspections 0}
       5 {:items [76] :op #(+ % 4) :test [13 6 3] :inspections 0}
       6 {:items [52 51 84 54] :op #(* % 17) :test [19 4 1] :inspections 0}
       7 {:items [82 86 91 79 94 92 59 94] :op #(+ % 7) :test [2 5 3] :inspections 0}))

(defn remove-first [pred coll n]
      (loop [new []
             old coll]
            (if (or (empty? old) (pred (first old)))
                (into new (rest old))
                (recur (conj new (first old)) (rest old)))))

(defn test-item [[divisible-by true-monkey false-monkey] value]
      (if (zero? (mod value divisible-by)) true-monkey false-monkey))

(defn remove-item [monkeys monkey-name item]
      (let [old-items (get-in monkeys [monkey-name :items])]
           (assoc-in monkeys [monkey-name :items] (remove-first #(= % item) old-items item))))

(defn add-item [monkeys monkey-name item]
      (let [old-items (get-in monkeys [monkey-name :items])]
           (assoc-in monkeys [monkey-name :items] (conj old-items item))))

(defn throw-item [monkeys from to new-item old-item]
      (-> monkeys
          (remove-item from old-item)
          (add-item to new-item)
          (update-in [from :inspections] inc)))

(defn inspect-item [{:keys [test op]} item]
      (let [new-item (Math/floorDiv ^long (op item) 3)
            throw-to (test-item test new-item)]
           [throw-to new-item item]))

(defn inspect [monkey]
      (map (partial inspect-item monkey) (monkey :items)))

(defn monkey-turn [monkeys monkey-name]
      (let [monkey (get monkeys monkey-name)
            item-throws (inspect monkey)]
           (reduce (fn [ms [receiver new-item old-item]]
                       (throw-item ms monkey-name receiver new-item old-item))
                   monkeys
                   item-throws)))

(defn monkey-round [monkeys]
      (reduce (fn [ms name] (monkey-turn ms name)) monkeys (sort (keys monkeys))))

(defn play-rounds [n monkeys]
      (last (take (inc n) (iterate monkey-round monkeys))))

(defn solve []
      (let [final-state (play-rounds 10000 actual-monkeys)
            inspection-counts (reverse (sort (map :inspections (vals final-state))))]
           (apply * (take 2 inspection-counts))))
