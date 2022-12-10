(ns aoc2022.rope-bridge
    (:require [aoc2022.loader :as loader]))

(defn load-actions [] (loader/puzzle-input-lines "day9.txt"))

(defn unpack-action [action-count]
      (let [[_ action count] (re-find #"([UDLR]) (\d+)" action-count)]
           (repeat (read-string count) action)))

(defn unpack-actions [actions] (flatten (map unpack-action actions)))

(defn up [[x y]] [x (inc y)])

(defn down [[x y]] [x (dec y)])

(defn right [[x y]] [(inc x) y])

(defn left [[x y]] [(dec x) y])

(def moves {"U" up "D" down "R" right "L" left})

(defn tail-move-function [[hx hy] [tx ty]]
      (let [dx (- hx tx)
            dy (- hy ty)]
           (case [dx dy]
                 [2 0]                      right
                 [0 2]                      up
                 [-2 0]                     left
                 [0 -2]                     down
                 ([1 2] [2 1] [2 2])        (comp right up)
                 ([-1 2] [-2 1] [-2 2])     (comp left up)
                 ([1 -2] [2 -1] [2 -2])     (comp right down)
                 ([-2 -1] [-1 -2] [-2 -2])  (comp left down)
                 identity)))

(defn move-head [head action]
      (let [move (get moves action)]
           (move head)))

(defn move-tail [head tail]
      (let [move (tail-move-function head tail)]
           (move tail)))

(defn move-rope [[head & tail] action]
      (let [new-head (move-head head action)]
           (reductions move-tail new-head tail)))

(defn rope [length] (vec (repeat length [0 0])))

(defn unique-tail-positions [moves] (count (set (map last moves))))

(defn solve []
     (let [actions (unpack-actions (load-actions))
           moves-one (reductions move-rope (rope 2) actions)
           moves-two (reductions move-rope (rope 10) actions)]
          (println (unique-tail-positions moves-one) (unique-tail-positions moves-two))))
