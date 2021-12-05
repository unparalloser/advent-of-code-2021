(ns unparalloser.advent-of-code-2021.day-04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def data
  (with-open [r (io/reader (io/resource "day_04_input.txt"))]
    (remove empty? (vec (line-seq r)))))

(defn split-ws [s]
  (str/split (str/triml s) #"\s+"))

(def bingo-numbers (map #(Integer/parseInt %) (str/split (first data) #",")))

(defn init-boards [rows]
  (->> rows
       (map #(zipmap % (repeat false)))
       (partition 5)))

(def bingo-boards
  (->> (rest data)
       (map (comp (partial map #(Integer/parseInt %)) 
                  split-ws))
       init-boards))

(defn rows->cols [board]
  (apply map (partial conj {}) board))

(defn assoc-if-contains [map key val]
    (if (contains? map key)
      (assoc map key val)
      map))

(defn mark-boards [boards n]
  (for [board boards]
    (for [row board]
      (assoc-if-contains row n true))))

(defn solved? [lines]
  (map (comp (partial every? true?) vals) lines))

(defn winner? [board]
  (let [rows-solved? (solved? board)
        cols-solved? (solved? (rows->cols board))]
    (some true? (concat rows-solved? cols-solved?))))

(defn winner [boards]
  (first (filter winner? boards)))

(defn bingo
  ([boards ns]
   (bingo boards ns nil))
  ([boards [n & ns] prev-n]
   (if-let [winner (winner boards)]
     [winner prev-n]
     (recur (mark-boards boards n) ns n))))

(defn filter-vals [pred coll]
  (filter (fn [[_ v]] (pred v)) coll))

(defn score [[board n]]
  (->> board
       (reduce concat)
       (filter-vals false?)
       keys
       (reduce +)
       (* n)))

(defn bingo-2
  ([boards ns]
   (bingo-2 boards ns nil))
  ([boards [n & ns :as numbers] prev-n]
   (if (= 1 (count boards))
     (bingo boards numbers prev-n)
     (recur (remove winner? (mark-boards boards n)) ns n))))

(defn -main []
  (println (score (bingo bingo-boards bingo-numbers)))    ; part 1
  (println (score (bingo-2 bingo-boards bingo-numbers)))) ; part 2
