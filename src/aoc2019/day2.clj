(ns aoc2019.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-input2 [filename]
  (->> (io/resource filename)
        (slurp)))

(defn restore-state [codes]
  (assoc (assoc codes 1 12) 2 2))

(defn process [codes]
  (loop [op-idx 0
         acc codes]
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

(defn run []
  (->> (str/split (get-input2 "day2.txt") #",")
    (map #(read-string %))
    (vec)
    (restore-state)
    (process)))
