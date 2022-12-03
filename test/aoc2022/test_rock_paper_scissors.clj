(ns aoc2022.test-rock-paper-scissors
    (:require [clojure.test :refer :all])
    (:require [aoc2022.rock-paper-scissors :as rps]))

(deftest test-strategy-one-scores
         (let [play (rps/play rps/strategy-one)]
              (are [a b score] (= (play a b) score)
                   "A" "Y" 8
                   "B" "X" 1
                   "C" "Z" 6)))

(deftest test-strategy-two-scores
         (let [play (rps/play rps/strategy-two)]
              (are [a b score] (= (play a b) score)
                   "A" "Y" 4
                   "B" "X" 1
                   "C" "Z" 7)))
