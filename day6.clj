(ns day6
  (:require [grid :as g]))

(defn blocked? [g pos]
  (= \# (g/at g pos)))

(def turn {:up :right :right :down :down :left :left :up})
(defn fwd [[x y] dir]
  (case dir
    :up [x (dec y)]
    :right [(inc x) y]
    :down [x (inc y)]
    :left [(dec x) y]))

(defn walk [g pos dir]
  "Walk one step or turn, returns new position and direction or nil
  if stepped outside the map."
  (let [np (fwd pos dir)]
    (cond
      (g/outside? g np) nil
      (blocked? g np) [pos (turn dir)]
      :else [np dir])))

(defn visited [g start dir]
  (loop [visited #{start}
         pos start
         dir dir]
    (if-let [[pos dir] (walk g pos dir)]
      (recur (conj visited pos) pos dir)
      visited)))

(defn part1 [g]
  (let [start (g/search g \^)]
    (count (visited g start :up))))

(defn loops?
  "Walk until we either step on a visited node in the same orientation
  or walk outside."
  [g pos dir]
  (loop [visited-dir #{}
         pos pos
         dir dir]
    (if-let [[pos dir] (walk g pos dir)]
      (if (visited-dir [pos dir])
        ;; loop detected, already was at this position facing the same direction
        true
        (recur (conj visited-dir [pos dir])
               pos dir))
      ;; walked outside, no loop
      false)))

(defn part2 [g]
  ;; Get all visited nodes in part 1, set that to obstruction and see if it loops
  (let [start (g/search g \^)
        g (g/set-at g start \.)
        visited (disj (visited g start :up) start)]
    (reduce concat
            (pmap #(let [g (g/set-at g % \#)]
                     (when (loops? g start :up)
                       [%])) visited))))

(time
 (let [g (g/grid "day6.txt")]
   (println "START: " (g/search g \^))
   (println "Part1: " (part1 g))
   (spit "day_clojure"
         (with-out-str
           (doseq [[x y] (sort (part2 g))]
             (println "Loop X: " x ", Y:" y))))))
