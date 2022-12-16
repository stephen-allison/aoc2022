(ns aoc2022.distress-signal-test
  (:require [clojure.test :refer :all])
  (:require [aoc2022.distress-signal :refer :all]))

(deftest two-simple-lists-left-has-smaller-entry
  (is (= :correct (packet-compare [1 1 3 1 1] [1 1 5 1 1]))))

(deftest two-simple-lists-left-shorter-but-has-smaller-entry
  (is (= :correct (packet-compare [1 1 3] [1 1 5 1 1]))))

(deftest left-list-is-shorter
  (is (= :correct (packet-compare [7 7 7] [7 7 7 7]))))

(deftest right-list-is-shorter
  (is (= :incorrect (packet-compare [7 7 7 7] [7 7 7]))))

(deftest equal-lists
  (is (= :inconclusive (packet-compare [1 1 1] [1 1 1]))))

(deftest one-empty-nested-list
  (is (= :correct (packet-compare [1 1 []] [1 1 1]))))

(deftest one-empty-list
  (is (= :correct (packet-compare [] [3]))))

(deftest nested-lists
  (is (= :correct (packet-compare [[1] [2 3 4]] [[1] 4]))))

(deftest nested-list-on-one-side
  (is (= :incorrect (packet-compare [9] [[8 7 6]]))))

(deftest almost-equal-lists
  (is (= :correct (packet-compare[[4 4] 4 4] [[4 4] 4 4 4]))))

(deftest nested-empty-lists
  (is (= :incorrect (packet-compare [[[]]] [[]]))))

(deftest more-complex-example
  (is (= :incorrect (packet-compare[1 [2 [3 [4 [5 6 7]]]] 8 9] [1 [2 [3 [4 [5 6 0]]]] 8 9]))))

(deftest equal-lists
  (is (= :inconclusive (packet-compare [[4 4] 4 4] [[4 4] 4 4]))))