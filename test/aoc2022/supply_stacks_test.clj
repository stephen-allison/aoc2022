(ns aoc2022.supply-stacks-test
  (:require [clojure.test :refer :all])
  (:require [aoc2022.supply-stacks :refer :all]))


(def initial-stacks [["Z" "N"] ["M" "C" "D"] ["P"]])

(deftest instruction-parsed-to-count-from-to-triple
         (is (= (parse-instruction "move 1 from 2 to 3") '(1 2 3))))

(deftest test-move-single
         (let [move (move-crates CRATE-MOVER-9000)]
              (is (= (move initial-stacks [1 2 1]) [["Z" "N" "D"] ["M" "C"] ["P"]]))))

(deftest test-move-two
         (let [move (move-crates CRATE-MOVER-9000)]
              (is (= (move initial-stacks [2 1 3]) [[] ["M" "C" "D"] ["P" "N" "Z"]]))))

(deftest test-move-two-super-crane
         (let [move (move-crates CRATE-MOVER-9001)]
              (is (= (move initial-stacks [2 1 3]) [[] ["M" "C" "D"] ["P" "Z" "N"]]))))