(ns aoc2019.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc2019.utils :as utils]))

(defn set-noun-and-verb [noun verb int-codes]
  (assoc (assoc int-codes 1 noun) 2 verb))

(defn process [int-codes]
  (loop [op-idx 0
         acc int-codes]
    (let [ops (partition 4 acc)
         [opcode pos1 pos2 output-pos] (nth ops op-idx)]
      (cond
        (== opcode 1)
          (recur (inc op-idx)
                 (assoc acc output-pos (+ (nth acc pos1) (nth acc pos2))))
        (== opcode 2)
          (recur (inc op-idx)
                 (assoc acc output-pos (* (nth acc pos1) (nth acc pos2))))
        (== opcode 99)
          acc
        :else
          (println "unknown" opcode)))))

(defn load-memory []
  (->> (str/split (utils/get-input "day2.txt") #",")
    (map #(read-string %))
    (vec)))

(defn gen-nouns-and-verbs []
  (for [x (range 0 100)
        y (range 0 100)]
  [x y]))

(let [memory (load-memory)]
  (->>
    (map
      (fn [[noun verb]]
        [noun verb (first (process (set-noun-and-verb noun verb memory)))])
      (gen-nouns-and-verbs))
    (filter (fn [[_ _ output]] (== output 19690720)))))

(+ (* 64 100) 29)
