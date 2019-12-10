(ns aoc2019.day4
  (:require [aoc2019.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn six-digit? [pw]
  (and (>= pw 100000) (<= pw 999999)))

(six-digit? 300000)
(six-digit? 123)

(>= 123 100000)
(defn digits [n]
  (->> n str (map (comp read-string str))))

(partition 4 1 (digits 123456))

(defn adjacent-digits? [size pw]
  (not (nil? (some true?
        (map (fn [[a b]] (= a b))
              (partition size 1 (digits pw)))))))

(adjacent-digits? 2 123456)
(adjacent-digits? 2 133456)
(adjacent-digits? 2 123466)
(adjacent-digits? 3 123666)

(filter #(adjacent-digits? 4 %) [112233 112223 112222])

(defn increasing-digits? [pw]
  (every? true? (map (fn [[a b]] (<= a b))
                        (partition 2 1 (digits pw)))))

(increasing-digits? 123456)
(increasing-digits? 113456)
(increasing-digits? 113436)

(defn meet-criteria [passwords]
  (filter #(increasing-digits? %)
    (filter #(adjacent-digits? 2 %)
      (filter #(six-digit? %) passwords))))

(defn meet-criteria-part2 [passwords]
  (filter
    (every-pred
      increasing-digits?
      (partial adjacent-digits? 2)
      six-digit?
      (complement (partial (adjacent-digits? 3)))
      (complement (partial (adjacent-digits? 4))))
    passwords))

(count (meet-criteria (range 125730 579381)))
(count (meet-criteria-part2 (range 125730 579381)))

