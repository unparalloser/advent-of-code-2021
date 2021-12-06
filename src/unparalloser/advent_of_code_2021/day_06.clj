(ns unparalloser.advent-of-code-2021.day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def initial-state
  (frequencies (map #(Integer/parseInt %)
       (str/split (slurp (io/resource "day_06_input.txt")) #","))))

(defn simulate-day
  ([map]
   (simulate-day map {}))
  ([[[k v] & map] new-map]
  (let [new-map (if (zero? k)
                  (-> new-map
                      (update 6 (fnil (partial + v) 0))
                      (update 8 (fnil (partial + v) 0)))
                  (update new-map (dec k) (fnil (partial + v) 0)))]
    (if (empty? map)
      new-map
      (recur map new-map)))))

(defn simulate-days [coll n]
  (if (zero? n)
    coll
    (recur (simulate-day coll) (dec n))))

(defn -main []
  (println (reduce + (vals (simulate-days initial-state 80))))   ; part 1
  (println (reduce + (vals (simulate-days initial-state 256))))) ; part 2