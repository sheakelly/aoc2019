(ns aoc2019.day2
  (:require [clojure.java.io :as io]))

(defn get-input [filename]
  (->> (io/resource filename)
        (slurp)))

(defn run []
  (->> (get-input "day2.txt")
        (println)))
