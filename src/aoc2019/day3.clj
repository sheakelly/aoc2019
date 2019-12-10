(ns aoc2019.day3
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

(plot-x 5 5 1 +)

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

(plot-paths [[["R" 8]["U" 5] ["L" 5] ["D" 3]]
             [["U" 7]["R" 6] ["D" 4] ["L" 4]]])

(defn intersections [coords]
  (apply set/intersection (map #(set %) coords)))

(defn calc-dist-from-origin [coords]
  (map (fn [[x y]] (+ (Math/abs(- 0 x)) (Math/abs(- 0 y)))) coords))

(defn run [paths]
  (apply min (calc-dist-from-origin
    (intersections
      (map #(:coords %) (plot-paths paths))))))

(defn calc-distances [path points]
  (loop [path' path
         points' points
         distances []]
    (let [[cur-point & next-points] points'
          [cur-path next-path] (split-at (+ (.indexOf path' cur-point) 1) path')
          distance (count cur-path)]
      (cond
        (nil? cur-point) distances
        (nil? cur-path) distances
        :else (recur path next-points (conj distances distance))))))

(def path [[0 1][0 2][0 3][0 4][0 5][0 6][0 7][0 8][0 9]])
(def points [[0 2][0 5][0 9]])

(calc-distances path points)

(defn step-lengths [paths]
  (let [plotted-paths (map #(:coords %) (plot-paths paths))
        inters (intersections plotted-paths)]
    (map #(calc-distances % inters) plotted-paths)))

(step-lengths [(parse-line "L5")
                   (parse-line "D1,L3,U5")])

(step-lengths [(parse-line "R8,U5,L5,D3")
                   (parse-line "U7,R6,D4,L4")])

(step-lengths [(parse-line "R75,D30,R83,U83,L12,D49,R71,U7,L72")
                   (parse-line "U62,R66,U55,R34,D71,R55,D58,R83")])

(run (get-wire-paths))

(run [(parse-line "R8,U5,L5,D3")
      (parse-line "U7,R6,D4,L4")])

(intersections [[[1 1] [1 2] [1 3] [1 4] [1 5]]
                [[1 1] [1 3] [1 4]]])

(calc-dist-from-origin [[1 1] [1 2] [5, 5]])

(set [[1 2] [1 2] [3 4]])

(set/intersection (set [[1 1] [1 2]]) (set [[1 2][3 4]]))
(set/intersection #{1 2} #{2 3})
