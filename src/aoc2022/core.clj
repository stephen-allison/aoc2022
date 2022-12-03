(ns aoc2022.core)
(require '[aoc2022.calorie-counting :as day1])
(require '[aoc2022.rock-paper-scissors :as day2])

(defn -main [& _]
      (println "Advent of code 2022!")
      (day1/solve)
      (day2/solve))