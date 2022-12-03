(ns aoc2022.calorie-counting)
(require '[aoc2022.loader :as loader])

(defn sum [xs] (reduce + xs))

(defn to-num [s] (Integer/parseInt s))

(defn items-by-elf [input-lines]
      "Returns a list where each item is a list containing the calorific
      values of one elf"
      (->> input-lines
           (partition-by empty?)
           (filter #(not (= % '(""))))
           (map (fn [strs] (map to-num strs)))))

(defn calorie-count [elf-items]
      (map sum elf-items))

(defn solve []
      (let [input (loader/puzzle-input-lines "day1.txt")
            food-items-by-elf (items-by-elf input)
            calorie-counts-by-elf (calorie-count food-items-by-elf)
            part1 (apply max calorie-counts-by-elf)
            part2 (sum (take 3 (reverse (sort calorie-counts-by-elf))))]
           (println part1 part2)))