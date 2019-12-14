(ns aoc2019.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc2019.utils :as utils]))

(defn load-int-codes []
  (->> (str/split (utils/get-input "day5.txt") #",")
    (map #(read-string %))
    (vec)))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn extract-digit [k n]
  (mod (int (/ k (exp 10 (- n 1)))) 10))

(defn decode-param-mode [value]
  (cond
    (= value 0) :position
    (= value 1) :immediate
    :else :unknown))

(defn decode-param-modes [ctx]
  (assoc ctx :param-modes
    (for [x [3 4 5]] (decode-param-mode (extract-digit (int-code-at-pointer ctx) x)))))

(decode-param-modes {:pointer 0 :int-codes [2]})

(defn int-code-at-pointer [ctx]
  (nth (:int-codes ctx) (:pointer ctx)))

(defn decode-op-code [ctx]
  (assoc ctx :op-code
    (let [value (log "value" (mod (int-code-at-pointer ctx) 100))]
      (cond
        (= value 1) :add
        (= value 2) :multiply
        (= value 3) :input
        (= value 4) :output
        (= value 99) :exit
        :else :unknown))))

(decode-op-code {:pointer 0 :int-codes [1002]})

(defmulti decode-param-count :op-code)
(defmethod decode-param-count :exit [ctx] (assoc ctx :param-count 0))
(defmethod decode-param-count :input [ctx] (assoc ctx :param-count 1))
(defmethod decode-param-count :output [ctx] (assoc ctx :param-count 1))
(defmethod decode-param-count :default [ctx] (assoc ctx :param-count 3))

(decode-param-count {:op-code :input})
(decode-param-count {:op-code :output})
(decode-param-count {:op-code :add})

(defn decode-instruction [ctx]
  (->> ctx
    (decode-param-modes)
    (decode-param-count)
    (resolve-params)))

(decode-instruction {:pointer 0 :int-codes [3 5 0 0 0 9]})
(decode-instruction {:pointer 0 :int-codes [1002]})

(defn resolve-param [input mode int-codes]
  (if (= mode :position)
    (nth int-codes input)
    input))

(defn resolve-params [ctx]
  ;(println "resolve-params ctx" ctx)
  (let [{:keys [op-code param-modes int-codes pointer param-count]} ctx
        inputs (take param-count (drop (+ pointer 1) int-codes))
        ctx' (assoc ctx :input-params inputs)]
    (assoc ctx' :resolved-params
       (map-indexed
         (fn [idx input]
            (resolve-param input (nth param-modes idx) int-codes))
         inputs))))

(resolve-params
  {:pointer 0 :op-code :input :param-modes [:position :position :position]
   :param-count 1
  :int-codes [3 5 0 0 0 9]})

(defmulti compute-inst :op-code)

(defmethod compute-inst :add
  [{[a b] :resolved-params
    int-codes :int-codes
    [_ _ output-pos] :input-params :as ctx }]
  (assoc ctx :int-codes (assoc int-codes output-pos (+ a b))))

(defmethod compute-inst :multiply
  [{[a b] :resolved-params
    int-codes :int-codes
    [_ _ output-pos] :input-params :as ctx }]
  (assoc ctx :int-codes (assoc int-codes output-pos (* a b))))

(defmethod compute-inst :input
  [{input :input
    int-codes :int-codes
    [output-pos] :input-params :as ctx }]
  (assoc ctx :int-codes (assoc int-codes output-pos input)))

(defmethod compute-inst :output
  [{[a] :resolved-params :as ctx}]
  (print a)
  ctx)

(defmethod compute-inst :default [ctx]
  ctx)

(compute-inst {:op-code :add
               :input-params [1 2 2]
               :resolved-params [1 2 3]
               :int-codes [0 0 0] })
(compute-inst {:op-code :multiply
               :input-params [3 3 2]
               :resolved-params [1 2 3]
               :int-codes [0 0 0] })

(defn move-pointer [ctx]
  (assoc ctx :pointer (next-pointer ctx)))

(defn next-pointer [{pointer :pointer :as ctx}]
  (+ pointer (:param-count ctx) 1))

(defn log [msg value]
  (println msg value) value)

(defn compute [input int-codes]
  (loop [ctx {:input input :pointer 0 :int-codes int-codes}]
    (if (<= (:pointer ctx) (count int-codes))
      (let [{op-code :op-code :as ctx'} (decode-op-code ctx)]
        (if (= :exit op-code)
          ctx
          (recur (->> ctx'
                  (decode-instruction)
                  (compute-inst)
                  (move-pointer)))))
      ctx)))


(compute 1 [3 0 4 0 99])
(compute 1 [1101 100 -1 4 0])
(count (load-int-codes))
(compute 1 (load-int-codes))
