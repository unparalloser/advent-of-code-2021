(ns unparalloser.advent-of-code-2021.day-05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def data
  (with-open [r (io/reader (io/resource "day_05_input.txt"))]
    (map (comp (partial map #(Integer/parseInt %))
               #(str/split % #" -> |,"))
         (vec (line-seq r)))))

(defn bidir-inc-range [n1 n2]
  (if (<= n1 n2)
    (range n1 (inc n2))
    (range n1 (dec n2) -1)))

(defn diagonal? [[x1 y1 x2 y2]]
  (not (or (= x1 x2)
           (= y1 y2))))

(defn lines->coords [data]
  (for [[x1 y1 x2 y2 :as v] data]
    (let [xs (bidir-inc-range x1 x2)
          ys (bidir-inc-range y1 y2)]
      (if (diagonal? v)
        (apply map vector [xs ys])
        (for [x xs
              y ys]
          [x y])))))

(defn filter-vals [pred coll]
  (filter (fn [[_ v]] (pred v)) coll))

(defn count-overlapping-coords [coords]
  (->> coords
       frequencies
       (filter-vals #(> % 1))
       count))

(defn -main []
  (let [coords (apply concat (lines->coords data))
        coords-wo-diagonals (apply concat (lines->coords (remove diagonal? data)))]
    (println (count-overlapping-coords coords-wo-diagonals)) ; part 1
    (println (count-overlapping-coords coords))))            ; part 2