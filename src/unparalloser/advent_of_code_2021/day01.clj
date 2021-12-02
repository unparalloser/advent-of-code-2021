(ns unparalloser.advent-of-code-2021.day01
  (:require [clojure.java.io :as io]))

(def data
  (with-open [r (io/reader (io/resource "day1_input.txt"))]
    (mapv #(Integer/parseInt %) (line-seq r))))

(defn count-inc
  ([nums] 
   (count-inc nums 0))
  ([[h & [n & _ :as t]] acc]
   (if (empty? t)
     acc
     (if (< h n)
       (recur t (inc acc))
       (recur t acc)))))

(def sliding-window-sums
  (map (partial reduce +) (partition 3 1 data)))

(defn -main []
  (println (count-inc data))                 ; part 1
  (println (count-inc sliding-window-sums))) ; part 2
