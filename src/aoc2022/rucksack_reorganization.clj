(ns aoc2022.rucksack-reorganization
    (:require [clojure.set :as set])
    (:require [aoc2022.loader :as loader]))

(defn compartments [rucksack]
      (let [length (/ (count rucksack) 2)]
           [(.substring rucksack 0 length)
            (.substring rucksack length)]))

(defn common-contents [comps]
      (apply set/intersection (map set comps)))

(defn priority [item]
      (let [code (int item)]
           (if (>= code 97) (- code 96) (- code 38))))

(defn common-priority [rucksack]
      (let [common (common-contents (compartments rucksack))
            item (first common)]
           (priority item)))

(defn elf-groups [rucksacks]
      (let [groups (partition 3 rucksacks)]
           (map common-contents groups)))

(defn solve []
      (let [rucksacks (loader/puzzle-input-lines "day3.txt")
            common (map common-priority rucksacks)
            part-one (reduce + common)
            groups (elf-groups rucksacks)
            group-scores (map (comp priority first) groups)
            part-two(reduce + group-scores)]
           (println part-one part-two)))