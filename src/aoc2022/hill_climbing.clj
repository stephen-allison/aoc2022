(ns aoc2022.hill-climbing
    (:require [aoc2022.loader :as loader])
    (:require [clojure.string :as str])
    (:require [clojure.set :as set]))

(def sample-map ["Sabqponm"
                 "abcryxxl"
                 "accszExk"
                 "acctuvwj"
                 "abdefghi"])

(def puzzle-map (loader/puzzle-input-lines "day12.txt"))

(def START \S)
(def END \E)
(def NO-DISTANCE (Integer/MAX_VALUE))
(defn up [[x y]] [x (dec y)])
(defn down [[x y]] [x (inc y)])
(defn right [[x y]] [(inc x) y])
(defn left [[x y]] [(dec x) y])
(def all-directions (juxt up right down left))
(defn neighbours [coord] (all-directions coord))

(defn char-to-height [c]
      (case c
            \S 1
            \E 26
            (- (int c) 96)))

(defn make-vertex [c] {:c c :h (char-to-height c) :prev nil :dist NO-DISTANCE})

(defn coordinate-seq [row-length]
     (map #(vector (mod % row-length) (Math/floorDiv % row-length)) (range)))

(defn read-vertices [map-data]
      (let [row-length (count (first map-data))
            chars (apply str map-data)
            verts (map make-vertex chars)
            indexed-chars (map vector (coordinate-seq row-length) verts)]
           (into {} indexed-chars)))

(defn starting-vertex [vertex-map]
      (first (filter #(= \S (:c (second %))) vertex-map)))

(defn start
      ([vertex-map]
        (start vertex-map (starting-vertex vertex-map)))
      ([vertex-map start-vertex]
        (let [[start-coord info] start-vertex]
              (assoc-in vertex-map [start-coord :dist] 0))))

(defn next-vertex [vertex-map]
      (first (sort-by #(:dist (second %)) vertex-map)))

(defn get-height [vertex-map coord] (get-in vertex-map [coord :h]))

(defn reachable-neighbours [vertex-map coord]
      (let [all-neighbours (neighbours coord)
            in-bounds (filter #(contains? vertex-map %) all-neighbours)
            current-h (get-height vertex-map coord)]
           (set (filter #(<= (- (get-height vertex-map %) current-h) 1) in-bounds))))

(defn update-neighbour [vertex-map [coord vertex] next-coord]
      (let [next-vertex (get vertex-map next-coord)
            candidate-dist (inc (:dist vertex))]
           (if (< candidate-dist (:dist next-vertex))
               ;;upate dist and prev
               (-> vertex-map
                   (assoc-in [next-coord :dist] candidate-dist)
                   (assoc-in [next-coord :prev] coord))
               ;;else
               vertex-map
               )))

(defn dijkstra-r [vertex-map visited-vertices]
      (loop [vmap vertex-map
             visited visited-vertices]
            (let [[coord vertex] (next-vertex vmap)
                  neighbours (reachable-neighbours vmap coord)
                  new-visited (conj visited [coord vertex])
                  new-vmap (dissoc vmap coord)]
                 (if (empty? new-vmap)
                     new-visited
                     (recur
                       (reduce #(update-neighbour %1 [coord vertex] %2) new-vmap neighbours)
                       new-visited)))))

(defn search-from-start [vertex-map]
      (let [start-map (start vertex-map)
            mapped (dijkstra-r start-map [])
            end (first (filter #(= \E (:c (second %))) mapped))]
           end
           ))

(defn search [vertex-map start-vertex]
      (let [start-map (start vertex-map start-vertex)
            mapped (dijkstra-r start-map [])
            end (first (filter #(= \E (:c (second %))) mapped))]
           end
           ))

(defn find-all-a [vertex-map]
      (filter #(= \a (:c (second %))) vertex-map))

(defn solve []
      (let [vmap (read-vertices puzzle-map)
            part-one (search-from-start vmap)
            part-two (pmap #(search vmap %) (find-all-a vmap))
            dists (map #(:dist (second %)) part-two)]
           (println (:dist (second part-one)))
           (println (apply min dists))
           (println "done")))
















