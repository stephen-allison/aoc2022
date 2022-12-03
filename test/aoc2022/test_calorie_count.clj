(ns aoc2022.test-calorie-count
    (:require [clojure.test :refer :all])
    (:require [aoc2022.calorie-counting :as cc]))

(def test-data [
                "1000"
                "2000"
                "3000"
                ""
                "4000"
                ""
                "5000"
                "6000"
                ""
                "7000"
                "8000"
                "9000"
                ""
                "10000"])

(deftest test-calorie-count
         (is (= (cc/calorie-count (cc/items-by-elf test-data))
                '(6000 4000 11000 24000 10000))))