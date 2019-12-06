(ns aoc2019.utils
  (:require [clojure.java.io :as io]))

(defn get-input [filename]
  (->> (io/resource filename)
        (slurp)))
