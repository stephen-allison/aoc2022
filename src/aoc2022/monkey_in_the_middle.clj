(ns aoc2022.monkey-in-the-middle)

(def sample-monkeys
     (sorted-map
       0 {:items (map bigint [79 98]) :op #(* % 19) :test [23 2 3] :inspections 0}
       1 {:items (map bigint [54 65 75 74]) :op #(+ % 6) :test [19 2 0] :inspections 0}
       2 {:items (map bigint [79 60 97]) :op #(* % %) :test [13 1 3] :inspections 0}
       3 {:items (map bigint [74]) :op #(+ % 3) :test [17 0 1] :inspections 0}))

(def actual-monkeys
     (sorted-map
       0 {:items (map bigint [50 70 54 83 52 78]) :op #(* % 3) :test [11 2 7] :inspections 0}
       1 {:items (map bigint [71 52 58 60 71]) :op #(* % %) :test [7 0 2] :inspections 0}
       2 {:items (map bigint [66 56 56 94 60 86 73]) :op #(+ % 1) :test [3 7 5] :inspections 0}
       3 {:items (map bigint [83 99]) :op #(+ % 8) :test [5 6 4] :inspections 0}
       4 {:items (map bigint [98 98 79]) :op #(+ % 3) :test [17 1 0] :inspections 0}
       5 {:items (map bigint [76]) :op #(+ % 4) :test [13 6 3] :inspections 0}
       6 {:items (map bigint [52 51 84 54]) :op #(* % 17) :test [19 4 1] :inspections 0}
       7 {:items (map bigint [82 86 91 79 94 92 59 94]) :op #(+ % 7) :test [2 5 3] :inspections 0}))

(def sample-super-divisor (* 23 19 13 17))
(def actual-super-divisor (* 11 7 3 5 17 13 19 2))
(def super-divisor actual-super-divisor)

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
           (assoc-in monkeys [monkey-name :items] (remove #(= % item) old-items))))

(defn add-item [monkeys monkey-name item]
      (let [old-items (get-in monkeys [monkey-name :items])]
           (assoc-in monkeys [monkey-name :items] (conj old-items item))))

(defn throw-item [monkeys from to new-item old-item]
      (-> monkeys
          (remove-item from old-item)
          (add-item to new-item)
          (update-in [from :inspections] inc)))

(defn inspect-item [{:keys [test op]} item]
      (let [new-item (op item)
            throw-to (test-item test new-item)]
           [throw-to new-item item]))

(defn inspect [monkey]
      (map (partial inspect-item monkey) (monkey :items)))

(defn monkey-turn [monkeys monkey-name]
      (let [monkey (get monkeys monkey-name)
            item-throws (inspect monkey)]
           (reduce (fn [ms [receiver new-item old-item]]
                       (throw-item ms monkey-name receiver (mod new-item super-divisor) old-item))
                   monkeys
                   item-throws)))

(defn monkey-round [monkeys]
      (reduce (fn [ms name] (monkey-turn ms name)) monkeys (sort (keys monkeys))))

(defn play-rounds [n monkeys]
      (println monkeys)
      (loop [rounds-left n
             state monkeys]
            (println rounds-left)
            (if (zero? rounds-left)
                state
                (recur (dec rounds-left) (monkey-round state)))))

(defn solve []
      (let [final-state (play-rounds 10000 actual-monkeys)
            inspection-counts (reverse (sort (map :inspections (vals final-state))))]
           (println final-state)
           (apply * (take 2 inspection-counts))))
