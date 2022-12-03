(ns aoc2022.rucksack-reorganization-test
  (:require [clojure.test :refer :all])
  (:require [aoc2022.rucksack-reorganization :as rr]))

(deftest test-compartment-contents
         (is (= (rr/compartments "vJrwpWtwJgWrhcsFMMfFFhFp")
                '("vJrwpWtwJgWr" "hcsFMMfFFhFp"))))

(deftest test-compartment-contents
         (is (= (rr/common-contents ["vJrwpWtwJgWr" "hcsFMMfFFhFp"])
                #{\p})))

(deftest test-compartment-contents
         (are [comp1 comp2 common] (= (rr/common-contents [comp1 comp2]) #{common})
              "vJrwpWtwJgWr" "hcsFMMfFFhFp" \p
              "PmmdzqPrV" "vPwwTWBwg" \P))

(deftest test-priority
         (are [item expected-priority]
              (= (rr/priority item) expected-priority)
              \a 1
              \z 26
              \A 27
              \Z 52))

(deftest test-priority-of-common-items
         (are [rucksack expected]
              (= (rr/common-priority rucksack) expected)
              "vJrwpWtwJgWrhcsFMMfFFhFp" 16
              "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL" 38
              "PmmdzqPrVvPwwTWBwg" 42
              "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn" 22
              "ttgJtRGJQctTZtZT" 20
              "CrZsJsPPZsGzwwsLwLmpwMDw" 19
              ))