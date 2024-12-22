(ns day22
  (:require [clojure.string :as str]))

(defn input []
  (->> "day22.txt" slurp str/split-lines (mapv Long/parseLong)))

(def ^:const MOD 16777216)

(defn next-secret [in]
  (let [s1 (mod (bit-xor in (* 64 in)) MOD)
        s2 (mod (bit-xor s1 (bit-shift-right s1 5)) MOD)]
    (mod (bit-xor s2 (* 2048 s2)) MOD)))

(defn part1 [numbers]
  (reduce + (map #(nth (iterate next-secret %) 2000) numbers)))

(defn diff-seqs [initial]
  (loop [values {}
         nums (take 2000 (map #(mod % 10) (iterate next-secret initial)))]
    (let [[a b c d e & _] nums]
      (if-not e
        values
        (let [ad (- b a)
              bd (- c b)
              cd (- d c)
              dd (- e d)
              key [ad bd cd dd]
              new-values (if (contains? values key)
                           values
                           (assoc values key e))]
          (recur new-values (rest nums)))))))

(defn part2 [numbers]
  (let [all-diff-seqs (map diff-seqs numbers)
        all-diffs (reduce into #{} (map keys all-diff-seqs))]
    (loop [best 0
           [ds & diffs] all-diffs]
      (if-not ds
        best
        (let [bananas (reduce + (map #(or (% ds) 0) all-diff-seqs))]
          (recur (max best bananas)
                 diffs))))))

(time
 (let [in (input)]
   (println "Part1: " (part1 in) "\n"
            "Part2: " (part2 in))))
