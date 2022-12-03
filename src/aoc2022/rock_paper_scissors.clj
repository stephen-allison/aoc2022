(ns aoc2022.rock-paper-scissors)
(require '[clojure.string :as str])
(require '[aoc2022.loader :as loader])

(defn sum [xs] (reduce + xs))

(def wins #{'("A" "Y") '("B" "Z") '("C" "X")})

(def draws #{'("A" "X") '("B" "Y") '("C" "Z")})

(def points {"X" 1 "Y" 2 "Z" 3})

(defn win [a b] (contains? wins (list a b)))

(defn draw [a b] (contains? draws (list a b)))

(defn play [move-chooser]
      (fn play-impl [a b]
          (let [move (move-chooser a b)]
               (cond (win a move) (+ (points move) 6)
                     (draw a move) (+ (points move) 3)
                     :else (points move)))))

(def moves {
            "X" {"A" "Z" "B" "X" "C" "Y"}
            "Y" {"A" "X" "B" "Y" "C" "Z"}
            "Z" {"A" "Y" "B" "Z" "C" "X"}})

(defn strategy-one [_ b] b)
(defn strategy-two [a b] (get-in moves [b a]))

(defn solve []
      (let [rounds (map #(str/split % #" ") (loader/puzzle-input-lines "day2.txt"))
            part1 (map #(apply (play strategy-one) %) rounds)
            part2 (map #(apply (play strategy-two) %) rounds)]
           (println (sum part1) (sum part2))))