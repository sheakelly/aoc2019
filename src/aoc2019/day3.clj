(ns aoc2019.day2
  (:require [aoc2019.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (->> (str/split line #",")
        (map (fn [y]
               [(subs y 0 1) (read-string (subs y 1 (count y)))]))))

(parse-line "R8,U5,L5,D3")

(defn get-wire-paths []
  (->> (utils/get-input "day3.txt")
    (str/split-lines)
    (map parse-line)))

(defn plot-x [x y dist mod-fn]
  (for [x' (range 1 (+ dist 1))] [(+ (mod-fn x') x) y]))

(defn plot-y [x y dist mod-fn]
  (for [y' (range 1 (+ dist 1))] [x (+ (mod-fn y') y)]))

(plot-x 5 5 10 +)

(defn plot [[dir dist] [x y]]
  (cond
    (= dir "L") (plot-x x y dist -)
    (= dir "R") (plot-x x y dist +)
    (= dir "U") (plot-y x y dist +)
    (= dir "D") (plot-y x y dist -)))


(plot ["R" 10] [0 0])
(plot ["L" 10] [5 5])
(plot ["U" 10] [5 5])
(plot ["D" 8] [0 0])

(defn plot-paths [paths]
  (map (fn [path]
    (reduce
      (fn [acc move]
        (let [{from :from
               coords :coords} acc
              plotted (plot move from)
              from' (last plotted)]
          {:from from' :coords (concat coords plotted)}))
      {:from [0 0] :coords []}
      path)) paths))

(plot-paths [[["R" 2]["U" 2]]
             [["L" 5]["D" 5]]])

(defn intersections [coords]
  (apply set/intersection (map #(set %) coords)))

(defn calc-dist-from-origin [coords]
  (map (fn [[x y]] (+ (Math/abs(- 0 x)) (Math/abs(- 0 y)))) coords))

(defn run [paths]
  (apply max (calc-dist-from-origin
    (intersections
      (map #(:coords %) (plot-paths paths))))))

(run (get-wire-paths))
(run [(parse-line "R8,U5,L5,D3")
      (parse-line "U7,R6,D4,L4")])

(intersections [[[1 1] [1 2] [1 3] [1 4] [1 5]]
                [[1 1] [1 3] [1 4]]])

(calc-dist-from-origin [[1 1] [1 2] [5, 5]])

(set [[1 2] [1 2] [3 4]])

(set/intersection (set [[1 1] [1 2]]) (set [[1 2][3 4]]))
(set/intersection #{1 2} #{2 3})
