(ns aoc2022.treetop-tree-house
    (:require [aoc2022.loader :as loader])
    (:require [clojure.string :as str])
    (:require [clojure.set :as set]))

(defn numberify-line [line] (for [s line] (Integer/parseInt (str s))))

(defn read-map []
      (let [lines (loader/puzzle-input-lines "day8.txt")]
           (map numberify-line lines)))

(defn make-grid-line [grid [y line]]
      (into grid (for [x (range (count line))] [[x y] (nth line x)])))

(defn make-grid [lines]
      (let [indexed-lines (map vector (range) lines)]
           (reduce make-grid-line {} indexed-lines)))

(defn scan-lines [grid]
      (let [[xmax ymax] (last (sort (keys grid)))
            left-to-right (for [y (range (inc ymax))] (for [x (range (inc xmax))] [x y]))
            top-to-bottom (for [x (range (inc xmax))] (for [y (range (inc ymax))] [x y]))
            all-lines [left-to-right top-to-bottom
                       (map reverse left-to-right) (map reverse top-to-bottom)]]
           (reduce #(into %1 %2) [] all-lines)))

(defn check-line-of-sight [grid line]
      (reduce (fn [[max locs] loc]
                  (if (> (grid loc) max) [(grid loc) (conj locs loc)] [max locs])) [-1 #{}] line))

(defn count-visible [grid]
      (let [lines (scan-lines grid)
            scans (map (partial check-line-of-sight grid) lines)]
           (count (apply set/union (map second scans)))))

(defn lines-of-sight [grid-size [x y]]
      [(reverse (for [xpos (range x)] [xpos y]))
       (for [xpos (range (inc x) grid-size)] [xpos y])
       (reverse (for [ypos (range y)] [x ypos]))
       (for [ypos (range (inc y) grid-size)] [x ypos])])

(defn view-distance [grid tree-loc line]
      (let [tree-height (grid tree-loc)]
           (reduce (fn [distance loc]
                       (if (< (grid loc) tree-height)
                           (inc distance)
                           (reduced (inc distance)))) 0 line)))

(defn scenic-score [grid loc]
      (let [grid-size (inc (first (last (sort (keys grid)))))
            los (lines-of-sight grid-size loc)
            distances (map (partial view-distance grid loc) los)]
           (apply * distances)))

(defn solve []
      (let [grid (make-grid (read-map))
            part-one (count-visible grid)
            part-two (apply max (pmap (partial scenic-score grid) (keys grid)))]
           (println part-one part-two)))