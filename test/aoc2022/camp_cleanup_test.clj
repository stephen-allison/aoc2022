(ns aoc2022.camp-cleanup-test
  (:require [clojure.test :refer :all])
  (:require [aoc2022.camp-cleanup :refer :all]))

(deftest test-string-converted-to-number-pair
         (is (= (make-pair "10-20") '(10 20))))

(deftest test-sets-are-created-from-number-pairs
         (is (= (assignment-sets '(1 3) '(2 4)) [#{1 2 3} #{2 3 4}])))

(deftest test-complete-overlaps-counted
         (is (= (find-overlaps [['(2 4) '(1 8)]
                                ['(2 4) '(3 5)]
                                ['(1 2) '(3 4)]] complete-overlap?) 1)))

(deftest test-any-overlaps-counted
         (is (= (find-overlaps [['(2 4) '(1 8)]
                                ['(2 4) '(3 5)]
                                ['(1 2) '(3 4)]] any-overlap?) 2)))
