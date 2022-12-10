(ns aoc2022.cathode-ray-tube
    (:require [clojure.string :as str])
    (:require [aoc2022.loader :as loader]))

(def NOOP "noop")

(defn noop[register] [register])

(defn addx [n register] [register (+ register n)])

(defn make-instruction [program-line]
      (if (= NOOP program-line)
          noop
          (let [[_ val] (str/split program-line #" ")
                addend (Integer/parseInt val)]
               (partial addx addend))))

(defn load-program []
      (map make-instruction (loader/puzzle-input-lines "day10.txt")))

(defn init [] [1])

(defn execute-program [ops]
      (reduce (fn [reg op] (into reg (op(last reg)))) (init) ops ))

(defn signal-strength [register-state]
      (let [indexed (vec (map vector (iterate inc 1) register-state))
            sample-points (into [20] (range 60 (count register-state) 40))
            signals (for [n sample-points] (apply * (nth indexed (dec n))))]
           (reduce + signals)))

(defn draw-pixel [[pixel sprite]]
      (if (contains? (set sprite) pixel) "#" "."))

(defn solve []
      (let [program (load-program)
            register-states (execute-program program)
            signal (signal-strength register-states)
            pixel-pos (map (juxt dec identity inc) register-states)
            render-data (vec (map vector (flatten (repeat (range 40))) pixel-pos))
            pixels (map draw-pixel render-data)
            lines (map #(apply str %) (partition 40 pixels))]
           (println signal)
           (doseq [line lines] (println line))))
