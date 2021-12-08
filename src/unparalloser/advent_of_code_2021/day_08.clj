(ns unparalloser.advent-of-code-2021.day-08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def data
  (with-open [r (io/reader (io/resource "day_08_input.txt"))]
    (map (comp (partial map #(str/split % #" "))
               #(str/split % #" \| "))
         (vec (line-seq r)))))

(defn count-unique-seg-digits [data]
  (->> data
       (mapcat second)  
       (filter #(contains? #{2 3 4 7} (count %)))
       count))

(defn decipher-pattern [pattern result]
  (case (count pattern)
    5 (cond
        (set/subset? (result \1) pattern)
        [\3 pattern]
        (= 3 (count (set/difference pattern (result \4))))
        [\2 pattern]
        :else
        [\5 pattern])
    6 (cond
        (not (set/subset? (set/intersection (result \2)
                                            (result \4)
                                            (result \5))
                          pattern))
        [\0 pattern]
        (set/superset? pattern (result \1))
        [\9 pattern]
        :else
        [\6 pattern])))

(defn decipher-patterns
  ([patterns]
   (let [patterns (sort-by count (map set patterns))]
     (decipher-patterns (drop-last (drop 3 patterns))
               {\1 (first patterns)
                \7 (second patterns)
                \4 (nth patterns 2)
                \8 (last patterns)})))
  ([patterns result]
   (if (empty? patterns)
     (set/map-invert result)
     (recur (rest patterns)
            (merge result (decipher-pattern (first patterns) result))))))

(defn decipher-output [[patterns output]]
  (let [patterns (decipher-patterns patterns)]
    (Integer/parseInt
     (str/join
      (map patterns (map set output))))))

(defn -main []
  (println (count-unique-seg-digits data))
  (println (reduce + (map decipher-output data))))