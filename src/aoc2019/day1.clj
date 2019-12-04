(ns aoc2019.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-input [filename]
  (->> (io/resource filename)
        (slurp)
        (str/split-lines)))

(defn calc-fuel [mass]
  (- (int (Math/floor (/ mass 3))) 2))

(defn run []
  (->> (get-input "day1.txt")
        (map #(read-string %))
        (map #(calc-fuel %))
        (reduce + 0)))
