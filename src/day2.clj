(ns day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def data
  (with-open [r (io/reader (io/resource "day2_input.txt"))]
    (doall
     (for [line (line-seq r)]
       (let [[act n] (str/split line #" ")]
         [(keyword act) (Integer/parseInt n)])))))

(defn act-to-pos [[act n]]
  (case act
    :down [0 n]
    :up [0 (- n)]
    :forward [n 0]))

(defn move-1 [ps]
  (apply map + (map act-to-pos ps)))

(println (reduce * (move-1 data))) ; part 1

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

(println (reduce * (move-2 data))) ; part 2