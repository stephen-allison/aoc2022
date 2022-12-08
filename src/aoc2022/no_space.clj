(ns aoc2022.no-space
    (:require [aoc2022.loader :as loader])
    (:require [clojure.string :as str]))

(def ROOT "/")

(def not-nil? (comp not nil?))

(defn start-fs [] {[ROOT] []})

(defn start-state [] {:fs (start-fs) :cwd [ROOT]})

(defn add-dir [fs parent new-name]
      (let [new-path (conj parent new-name)]
           (if (contains? fs new-path)
               fs
               (assoc fs new-path []))))

(defn get-dir [fs path] (get fs path))

(defn file [name size] {:name name :size (read-string size)})

(defn add-file [fs dir file]
      (let [dir-contents (get-dir fs dir)]
           (assoc fs dir (conj dir-contents file))))

(defn sub-dirs [fs dir]
      (set (filter #(= dir (take (count dir) %)) (keys fs))))

(defn dir-size [fs dir]
      (let [dirs (sub-dirs fs dir)
            sum-fn (fn [fs dir] (reduce + (map :size (get-dir fs dir))))]
           (reduce + (map (partial sum-fn fs) dirs))))

(defn update-cwd [cwd path-el]
      (cond (= ".." path-el) (if (= [ROOT] cwd)
                                 cwd
                                 (vec (drop-last cwd)))
            :else (conj cwd path-el)))

(defn follow-path [cwd path]
      (let [els (str/split path #"/")]
           (reduce update-cwd cwd els)))

(defn cd [to-str cwd]
      (cond (= ROOT to-str) [ROOT]
            (= ".." to-str) (update-cwd cwd to-str)
            (str/starts-with? to-str "/") (into [ROOT] (rest (str/split to-str #"/")))
            :else (follow-path cwd to-str)
            ))

(defn try-reader [command [re cmd]]
     (let [match (re-find re command)]
          (if match
              [cmd (rest match)]
              nil)))

(defn do-cd [state path]
      (assoc state :cwd (cd (first path) (state :cwd))))

(defn do-add-file [state [size name]]
      (assoc state :fs (add-file (state :fs) (state :cwd) (file name size))))

(defn do-add-dir [state [name]]
      (assoc state :fs (add-dir (state :fs) (state :cwd) name)))

(def readers [[#"cd (.*)" do-cd]
              [#"(\d+) (.*)" do-add-file]
              [#"dir (.*)" do-add-dir]])

(defn do-command [state command]
      (let [[cmd args] (first (filter not-nil? (map (partial try-reader command) readers)))]
           (if cmd (apply cmd (conj [state] args))
                   state)))

(defn solve []
      (let [commands (loader/puzzle-input-lines "day7.txt")
            state (reduce do-command (start-state) commands)
            sizes (map (partial dir-size (state :fs)) (keys (state :fs)))
            result-part-1 (reduce + (filter #(< % 100000) sizes))
            disk-space 70000000
            used-space (dir-size (state :fs) [ROOT])
            free-space (- disk-space used-space)
            required-space 30000000
            to-free (- required-space free-space)
            result-part-2 (apply min (filter #(> % to-free) sizes))]
           (println (str result-part-1) result-part-2)))