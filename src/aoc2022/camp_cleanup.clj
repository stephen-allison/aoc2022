(ns aoc2022.camp-cleanup
    (:require [aoc2022.loader :as loader])
    (:require [clojure.string :as str])
    (:require [clojure.set :as set]))

(defn make-pair [pair-str]
      (map read-string (str/split pair-str #"-")))

(defn cleaning-assignments []
      (->> (loader/puzzle-input-lines "day4.txt")
           (map #(str/split % #","))
           (map (partial map make-pair))))

(defn assignment-sets [[start-1 end-1] [start-2 end-2]]
      (let [locations-1 (set (range start-1 (inc end-1)))
             locations-2 (set (range start-2 (inc end-2)))]
            [locations-1 locations-2]))

(defn complete-overlap? [assignment-set-1 assignment-set-2]
      (let [[smaller larger] (sort-by count [assignment-set-1 assignment-set-2])]
           (set/subset? smaller larger)))

(defn any-overlap? [assignment-set-1 assignment-set-2]
      (not (empty? (set/intersection assignment-set-1 assignment-set-2))))

(defn find-overlaps [assignments overlap-finder]
      (->> assignments
           (map (partial apply assignment-sets))
           (map (partial apply overlap-finder))
           (filter true?)
           (count)))

(defn solve []
      (let [assignments (cleaning-assignments)
            overlap-count (find-overlaps assignments complete-overlap?)
            total-overlap (find-overlaps assignments any-overlap?)]
           (println overlap-count total-overlap)))