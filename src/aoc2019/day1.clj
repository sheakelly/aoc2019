(ns aoc2019.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-input [filename]
  (->> (io/resource filename)
        (slurp)
        (str/split-lines)))

(defn fuel-for-mass [mass]
  (- (int (Math/floor (/ mass 3))) 2))

(defn fuel-for-module [module]
  (loop [total 0, next-mass module]
    (let [mass (fuel-for-mass next-mass)]
      (if (<= mass 0)
        total
        (recur (+ total mass), mass)))))

(defn run []
  (->> (get-input "day1.txt")
        (map #(read-string %))
        (map #(fuel-for-module %))
        (reduce + 0)))
