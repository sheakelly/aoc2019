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

(partition 2 1 (digits 123456))

(defn adjacent-digits? [pw]
  (not (nil? (some true?
        (map (fn [[a b]] (= a b))
              (partition 2 1 (digits pw)))))))

(adjacent-digits? 123456)
(adjacent-digits? 133456)
(adjacent-digits? 123466)

(defn increasing-digits? [pw]
  (every? true? (map (fn [[a b]] (<= a b))
                        (partition 2 1 (digits pw)))))

(increasing-digits? 123456)
(increasing-digits? 113456)
(increasing-digits? 113436)

(defn meet-criteria [passwords]
  (filter #(increasing-digits? %)
    (filter #(adjacent-digits? %)
      (filter #(six-digit? %) passwords))))

(count (meet-criteria (range 125730 579381)))

