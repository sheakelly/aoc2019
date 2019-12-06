(ns aoc2019.day2
  (:require [aoc2019.utils :as utils]
            [clojure.string :as str]))

(->> (utils/get-input "day3.txt")
  (str/split-lines)
  (map #(str/split % #","))
  (map (fn [x]
         (map (fn [dir & cnt] [dir cnt]) x))))

