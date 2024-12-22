(ns day20
  (:require [grid :as g])
  (:import (java.util PriorityQueue)))

(defn a*
  "Calculate shortest path. Returns seq of positions comprising the path.
  Returns nil of no path is found."
  [g from to]
  (let [h (partial g/distance to)
        gscore (volatile! {from 0})
        fscore (volatile! {from (h from)})
        gs #(get @gscore % Long/MAX_VALUE)
        fs #(get @fscore % Long/MAX_VALUE)
        gs! (fn [pos s] (vswap! gscore assoc pos s))
        fs! (fn [pos s] (vswap! fscore assoc pos s))
        came-from (volatile! {})
        came-from! (fn [p1 p2] (vswap! came-from assoc p1 p2))
        open (PriorityQueue. (comparator
                              (fn [a b]
                                (< (fs a) (fs b)))))]
    (.add open from)
    (loop [[x y :as at] (.poll open)]
      (when at
        (if (= [x y] to)
          ;; reconstruct path
          (reverse (take-while some? (iterate @came-from to)))

          ;; not yet at destination
          (do
            (doseq [n (g/neighbors g [x y])
                    :when (not= \# (g/at g n))
                    :let [tentative-gscore (inc (gs [x y]))
                          new-fs (+ tentative-gscore (h n))]
                    :when (< tentative-gscore (gs n))]
              (gs! n tentative-gscore)
              (fs! n new-fs)
              (came-from! n [x y])
              (when-not (.contains open n)
                (.add open n)))
            (recur (.poll open))))))))

(defn input []
  (g/grid "day20.txt"))

(defn shortest-path [g]
  (a* g (g/search g \S) (g/search g \E)))

(defn cheat-positions
  "Determine cheat positions. A cheat position is any wall that
  connects 2 positions on the shortest path in 2 moves."
  [g short-path]
  (let [shortest (set short-path)
        cheats (set (for [p shortest
                          n (g/neighbors g p)
                          :when (and (= \# (g/at g n))
                                     (> (count (filter shortest (g/neighbors g n))) 1))]
                      n))]
    cheats))

(defn cheat-length [g cheat]
  (count (shortest-path (g/set-at g cheat \.))))

(defn part1 [g]
  (let [short-path (shortest-path g)
        short-len (count short-path)]
    (loop [[c & cs] (cheat-positions g short-path)
           save100 0]
      (if-not c
        save100
        (recur cs
               (if (>= (- short-len (cheat-length g c)) 100)
                 (inc save100)
                 save100))))))

(defn heads [l]
  (if (seq l)
    (lazy-seq
     (cons l
           (heads (rest l))))))

(defn part2
  ([g] (part2 g 100))
  ([g save-amount]
   (let [short-path (shortest-path g)]
     (loop [[p & pos] short-path
            save100 0]
       (let [rem-len (count pos)]
         (if (< rem-len save-amount)
           save100
           ;; Drop first positions, because we can't save 100 moves from those
           (let [cheats (count (keep (fn [[p1 :as path]]
                                       (let [d (g/distance p p1)
                                             len (dec (count path))]
                                         (when (and (<= d 20)
                                                    (>= (- rem-len (+ d len)) save-amount))
                                           p1)))
                                     (heads (drop save-amount pos))))]
             (recur pos (+ save100 cheats)))))))))
