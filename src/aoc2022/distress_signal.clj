(ns aoc2022.distress-signal
  (:require [aoc2022.loader :as loader]))

(defn packets []
  (->> (loader/puzzle-input-lines "day13.txt")
       (filter #(not= "" %))
       (map (comp eval read-string))))

(def INT-TYPE (type 1))
(def VEC-TYPE (type []))

(defn int-compare [left right]
  (cond (< left right) :correct
        (> left right) :incorrect
        :else :inconclusive))

(defmulti packet-compare (fn [l r] [(type l) (type r)]))

(defn list-compare [left-list right-list]
  (loop [left left-list
         right right-list]
    (let [left-val (first left)
          right-val (first right)
          comparison (packet-compare left-val right-val)]
      (cond (and (nil? left-val) (nil? right-val)) :inconclusive
            (not= :inconclusive comparison) comparison
            :else (recur (rest left) (rest right))
            ))))

(defn mixed-compare-l [left-list right] (packet-compare left-list [right]))
(defn mixed-compare-r [left right-list] (packet-compare [left] right-list))


(defmethod packet-compare [INT-TYPE INT-TYPE] [left right] (int-compare left right))
(defmethod packet-compare [VEC-TYPE VEC-TYPE] [left right] (list-compare left right))
(defmethod packet-compare [VEC-TYPE INT-TYPE] [left right] (mixed-compare-l left right))
(defmethod packet-compare [INT-TYPE VEC-TYPE] [left right] (mixed-compare-r left right))
(defmethod packet-compare [nil nil] [_ _] :inconclusive)
(defmethod packet-compare [nil INT-TYPE] [_ _] :correct)
(defmethod packet-compare [INT-TYPE nil] [_ _] :incorrect)
(defmethod packet-compare [nil VEC-TYPE] [_ _] :correct)
(defmethod packet-compare [VEC-TYPE nil] [_ _] :incorrect)

(defn solve []
  (->> (packets)
       (partition 2)
       (map #(apply packet-compare %))
       (map vector (range))
       (filter #(= :correct (second %)))
       (map (comp inc first))
       (reduce +)))

(defn packet-compare-all [packets]
  (loop [pkts packets
         results []]
    (if (nil? (second pkts))
      results
      (let [l (first pkts)
            r (second pkts)]
        (recur (rest pkts) (conj results (packet-compare l r)))
        ))))

(defn count-correct [results]
  (count (filter #(= :correct %) results)))

(defn packet-rank [packet packets]
  (map #(packet-compare packet %) packets))

(defn packet-following [packets]
  (map #(packet-rank % packets) packets))

(defn solve-part-two []
  (let [pks (into (packets) [[[2]] [[6]]])
        following (packet-following pks)
        follower-counts (map count-correct following)
        packet-pos (apply hash-map (interleave pks follower-counts))
        packet-count (count pks)]
    (apply * [(- packet-count (get packet-pos [[2]]))
              (- packet-count (get packet-pos [[6]]))])))