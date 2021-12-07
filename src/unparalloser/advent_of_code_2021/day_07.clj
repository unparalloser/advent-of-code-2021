(ns unparalloser.advent-of-code-2021.day-07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def data
  (map #(Integer/parseInt %)
       (str/split (slurp (io/resource "day_07_input.txt")) #",")))

(defn median [coll]
  (nth (sort coll) (/ (count coll) 2)))

(defn mean-floor [coll]
  (quot (reduce + coll) (count coll)))

(defn steps [xs dest]
  (map #(Math/abs (- % dest)) xs))

(defn triangle-number
  "Like a factorial, but with addition"
  [x]
  (reduce + (range 1 (inc x))))

(defn fuel [xs dest]
  (reduce + (map triangle-number (steps xs dest))))

(defn min-fuel [xs]
  (let [mean-floor (mean-floor xs)
        fuel-floor (fuel xs mean-floor)
        fuel-ceiling (fuel xs (inc mean-floor))]
    (min fuel-floor fuel-ceiling)))

(defn -main []
  (println (reduce + (steps data (median data)))) ; part 1
  (println (min-fuel data)))                      ; part 2