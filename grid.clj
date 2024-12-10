(ns grid
  "ASCII grid utilities "
  (:require [clojure.string :as str]))

(defn grid
  [f]
  (let [lines (str/split-lines (slurp f))
        max-y (dec (count lines))
        max-x (dec (count (first lines)))]
    {:max-x max-x :max-y max-y
     :g
     (fn [[x y]]
       (when (and (<= 0 x max-x)
                  (<= 0 y max-y))
         (.charAt (nth lines y) x)))}))

(defn set-at
  "Return grid with pos overridden with given value."
  [{g :g :as grid} pos what]
  (assoc grid :g
         (fn [pos1]
           (if (= pos1 pos)
             what
             (g pos1)))))

(defn at
  "Get value at pos"
  [{g :g} pos]
  (g pos))

(defn search [{:keys [max-x max-y g]} what]
  (loop [y 0
         x 0]
    (when (<= y max-y)
      (if (= what (g [x y]))
        [x y]
        (if (= x max-x)
          (recur (inc y) 0)
          (recur y (inc x)))))))

(defn outside? [{:keys [max-x max-y]} [x y]]
  (or (not (<= 0 x max-x))
      (not (<= 0 y max-y))))
