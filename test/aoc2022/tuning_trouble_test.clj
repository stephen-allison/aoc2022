(ns aoc2022.tuning-trouble-test
  (:require [clojure.test :refer :all])
  (:require [aoc2022.tuning-trouble :refer :all]))

(deftest test-find-start-sequence
         (are [code expected-start-position]
              (= expected-start-position (find-start-position code 4))
              "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 7
              "bvwbjplbgvbhsrlpgdmjqwftvncz" 5
              "nppdvjthqldpwncqszvftbrmjlhg" 6
              "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 10
              "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 11))