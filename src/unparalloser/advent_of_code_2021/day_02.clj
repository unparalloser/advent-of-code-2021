(ns unparalloser.advent-of-code-2021.day-02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def data
  (with-open [r (io/reader (io/resource "day_02_input.txt"))]
    (doall
     (for [line (line-seq r)]
       (let [[act n] (str/split line #" ")]
         [(keyword act) (Integer/parseInt n)])))))

(defn act-to-pos [[act n]]
  (case act
    :down [0 n]
    :up [0 (- n)]
    :forward [n 0]))

(defn move [ps]
  (apply map + (map act-to-pos ps)))

(defn move-2
  ([moves]
   (move-2 moves 0 [0 0]))
  ([[[act n] & tail :as moves] aim pos]
   (if (empty? moves)
     pos
     (case act
       :down (recur tail (+ aim n) pos)
       :up (recur tail (- aim n) pos)
       :forward (recur tail aim (map + pos [n (* aim n)]))))))

(defn -main []
  (println (reduce * (move data)))    ; part 1
  (println (reduce * (move-2 data)))) ; part 2