(ns unparalloser.advent-of-code-2021.day-03
  (:require [clojure.java.io :as io]))

(def data
  (with-open [r (io/reader (io/resource "day_03_input.txt"))]
    (vec (line-seq r))))

(defn modes-by [coll pred]
  (->> coll frequencies (sort-by val pred) (partition-by val) first (map key)))

(defn bit-criteria [coll pred default]
  (let [modes (modes-by coll pred)]
    (or (some #{default} modes)
        (first modes))))

(defn bin->int [cs]
  (Integer/parseInt (reduce str cs) 2))

(defn rate [xs pred default]
  (bin->int
   (for [x (apply map vector xs)]
     (bit-criteria x pred default))))

(defn rate-2
  ([xs pred default]
   (rate-2 xs pred default 0))
  ([xs pred default pos]
   (if (= 1 (count xs))
     (bin->int (first xs))
     (let [col (map #(nth % pos) xs)
           bit-criteria (bit-criteria col pred default)
           xs (filter #(= bit-criteria (nth % pos)) xs)]
       (recur xs pred default (inc pos))))))

(defn -main []
  (let [gamma-rate (rate data > \1)
        epsilon-rate (rate data < \0)]
    (println (* gamma-rate epsilon-rate)))         ; part 1
  
  (let [o2-gen-rating (rate-2 data > \1)
        co2-scrub-rating (rate-2 data < \0)]
    (println (* o2-gen-rating co2-scrub-rating)))) ; part 2