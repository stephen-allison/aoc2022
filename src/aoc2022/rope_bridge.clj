(ns aoc2022.rope-bridge
    (:require [aoc2022.loader :as loader]))

(defn load-actions []
      (loader/puzzle-input-lines "day9.txt"))

(defn unpack-action [action-count]
      (let [[_ action count] (re-find #"([UDLR]) (\d+)" action-count)]
           (repeat (read-string count) action)))

(defn unpack-actions [actions]
      (flatten (map unpack-action actions)))

;; ..aDb..
;; .c...d.
;; R..H..L
;; .e...f.
;; ..gUh..
(defn up [[x y]] [x (inc y)])

(defn down [[x y]] [x (dec y)])

(defn right [[x y]] [(inc x) y])

(defn left [[x y]] [(dec x) y])

(def head-moves {
                 "U" up
                 "D" down
                 "R" right
                 "L" left
                 })

(defn tail-move-function [[hx hy] [tx ty]]
      (let [dx (- hx tx)
            dy (- hy ty)]
           (case [dx dy]
                 ;; R
                 [2  0] right
                 ;; U
                 [0  2] up
                 ;; L
                 [-2 0] left
                 ;; D
                 [0 -2] down
                 ;; g
                 [1  2] (comp up right)
                 ;; h
                 [-1 2] (comp left up)
                 ;; e
                 [2  1] (comp up right)
                 ;; f
                 [-2 1] (comp left up)
                 ;; c
                 [2 -1] (comp right down)
                 ;; d
                 [-2 -1] (comp left down)
                 ;; a
                 [1 -2] (comp right down)
                 ;; b
                 [-1 -2] (comp left down)

                 [2  2] (comp up right)
                 [-2 2] (comp up left)
                 [2 -2] (comp down right)
                 [-2 -2] (comp down left)
                 ;; else
                 identity)))

(defn new-head-position [head action]
      (let [move (get head-moves action)]
           (move head)))

(defn new-tail-position [head tail]
      (let [move (tail-move-function head tail)]
           (move tail)))

(defn move-rope [[head tail] action]
      (let [new-head (new-head-position head action)
            new-tail (new-tail-position new-head tail)]
           [new-head new-tail]))

(defn move-long-rope [[head & tail] action]
      (let [new-head (new-head-position head action)]
           (reductions new-tail-position new-head tail)))

(defn start [] [[0 0] [0 0]])

(defn start-part-two []  (into [] (repeat 10 [0 0])))

(def sample-actions ["R 4" "U 4" "L 3" "D 1" "R 4" "D 1" "L 5" "R 2"])

(def sample-actions-two ["R 5" "U 8" "L 8" "D 3" "R 17" "D 10" "L 25" "U 20"])

(defn solve-part-one []
     (let [actions (unpack-actions (load-actions))
           moves (reductions move-rope (start) actions)]
          (count (set (map second moves)))))

(defn solve-part-two []
      (let [actions (unpack-actions (load-actions))
            moves (reductions move-long-rope (start-part-two) actions)]
           (count (set (map last moves)))))