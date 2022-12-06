(ns aoc2022.tuning-trouble
    (:require [aoc2022.loader :as loader]))

(defn get-code [] (loader/puzzle-input "day6.txt"))

(defn window [n coll]
      (cons (take n coll) (lazy-seq (window n (rest coll)))))

(defn any-same? [coll] (not= (count coll) (count (set coll))))

(defn find-start-position [code block-size]
      (let [indexed-code (map vector (range) (window block-size (seq code)))
            [index letters] (first (drop-while (comp any-same? second) indexed-code))]
           (+ index (count letters))))

(defn solve []
      (let [code (get-code)
            start-code (find-start-position code 4)
            message (find-start-position code 14)]
           (println start-code message)))

