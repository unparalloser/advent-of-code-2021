(ns unparalloser.advent-of-code-2021.day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def initial-state
  (frequencies
   (map #(Integer/parseInt %)
        (str/split (slurp (io/resource "day_06_input.txt")) #","))))

(defn increase [map k v]
  (update map k (fnil (partial + v) 0)))

(defn simulate-day
  ([map]
   (simulate-day map {}))
  ([[[k v] & map] new-map]
   (let [new-map (if (zero? k)
                   (-> new-map
                       (increase 6 v)
                       (increase 8 v))
                   (increase new-map (dec k) v))]
     (if (empty? map)
       new-map
       (recur map new-map)))))

(defn fish-sum [coll n]
  (reduce + (vals (nth (iterate simulate-day coll) n))))

(defn -main []
  (println (fish-sum initial-state 80))   ; part 1
  (println (fish-sum initial-state 256))) ; part 2