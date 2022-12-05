(ns aoc2022.supply-stacks
    (:require [aoc2022.loader :as loader]))

(def start [
            ["B" "P" "N" "Q" "H" "D" "R" "T"]
            ["W" "G" "B" "J" "T" "V"]
            ["N" "R" "H" "D" "S" "V" "M" "Q"]
            ["P" "Z" "N" "M" "C"]
            ["D" "Z" "B"]
            ["V" "C" "W" "Z"]
            ["G" "Z" "N" "C" "V" "Q" "L" "S"]
            ["L" "G" "J" "M" "D" "N" "V"]
            ["T" "P" "M" "F" "Z" "C" "G"]
            ])

(def CRATE-MOVER-9000 reverse)
(def CRATE-MOVER-9001 identity)

(defn parse-instruction [instruction-str]
      (map read-string (re-seq #"\d+" instruction-str)))

(defn load-moves []
      (->> (loader/puzzle-input-lines "day5.txt")
           (filter #(re-matches #"^move.*" %))
           (map parse-instruction)))

(defn crate-mover [crane-fn]
      (fn [stacks index from-index to-index moved]
          (let [stack (get stacks index)]
               (cond (= index from-index) (vec (drop-last (count moved) stack))
                     (= index to-index) (into stack (crane-fn moved))
                     :else stack))))

(defn move-crates [mover]
      (fn [stacks [number from to]]
            (let [from-index (dec from)
                  to-index (dec to)
                  from-stack (get stacks from-index)
                  moved (take-last number from-stack)]
                 (vec (for [index (range (count stacks))]
                           (mover stacks index from-index to-index moved))))))

(defn solve []
      (let [moves (load-moves)
            part-1 (reduce (move-crates (crate-mover CRATE-MOVER-9000)) start moves)
            part-2 (reduce (move-crates (crate-mover CRATE-MOVER-9001)) start moves)]
           (println (apply str (map last part-1)))
           (println (apply str (map last part-2)))))