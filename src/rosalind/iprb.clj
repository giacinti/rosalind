(ns rosalind.iprb
  (:require [rosalind.core :refer [def-rosalind-main dataset-seq]]))

(defn dominant-prob [k m n]
  (/ (+ (* k (dec k)) (* 2 k m) (* 2 k n) (* m n) (* (/ 3 4) m (dec m)))
     (* (+ k m n) (- (+ k m n) 1))))

(defn -rosalind-main [file]
  (let [vals (map read-string
                  (clojure.string/split
                   (first (dataset-seq file))
                   #"\s+"))
        result (apply dominant-prob vals)]
    (format "%.05f" (+ result 0.0))))

(def-rosalind-main iprb -rosalind-main)
