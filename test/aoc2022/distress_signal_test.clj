(ns aoc2022.distress-signal-test
  (:require [clojure.test :refer :all])
  (:require [aoc2022.distress-signal :refer :all]))

(deftest two-simple-lists-left-has-smaller-entry
  (is (= :correct (list-compare [1 1 3 1 1] [1 1 5 1 1]))))

(deftest two-simple-lists-left-shorter-but-has-smaller-entry
  (is (= :correct (list-compare [1 1 3] [1 1 5 1 1]))))

(deftest left-list-is-shorter
  (is (= :incorrect (list-compare [7 7 7] [7 7 7 7]))))

(deftest right-list-is-shorter
  (is (= :correct (list-compare [7 7 7 7] [7 7 7]))))

(deftest equal-lists
  (is (= :inconclusive (list-compare [1 1 1] [1 1 1]))))