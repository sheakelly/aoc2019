(ns aoc2019.day2
  (:require [aoc2019.utils :as utils]
            [clojure.string :as str]))

(defn wire-paths []
  (->> (utils/get-input "day3.txt")
    (str/split-lines)
    (map #(str/split % #","))
    (map (fn [x]
           (map (fn [y] [(subs y 0 1) (read-string (subs y 1 (count y)))]) x)))))

(defn plot-x [x y dist mod-fn]
  (for [x' (range 1 (+ dist 1))] [(+ (mod-fn x') x) y]))

(defn plot-y [x y dist mod-fn]
  (for [y' (range 1 (+ dist 1))] [x (+ (mod-fn y') y)]))

(plot-x 5 5 10 +)

(defn plot [[dir dist] [x y]]
  (println "dir" dir "dist" dist "x" x "y" y)
  (cond
    (= dir "L") (plot-x x y dist -)
    (= dir "R") (plot-x x y dist +)
    (= dir "U") (plot-y x y dist +)
    (= dir "D") (plot-y x y dist -)))


(plot ["R" 10] [5 5])
(plot ["L" 10] [5 5])
(plot ["U" 10] [5 5])
(plot ["D" 8] [0 0])

(wire-paths)
