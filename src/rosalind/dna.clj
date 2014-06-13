(ns rosalind.dna
  (:require [rosalind.core :refer [def-rosalind-main]]))

;; (defn count_next_nucleotides [str]
;;   (if (or (empty? str) (= (first str) \newline))
;;     [0 0 0 0]
;;     (let [base (first str)
;;           vec (count_next_nucleotides (rest str))]
;;       (condp = base
;;         \A [(inc (get vec 0)) (get vec 1) (get vec 2) (get vec 3)]
;;         \C [(get vec 0) (inc (get vec 1)) (get vec 2) (get vec 3)]
;;         \G [(get vec 0) (get vec 1) (inc (get vec 2)) (get vec 3)]
;;         \T [(get vec 0) (get vec 1) (get vec 2) (inc (get vec 3))]
;;         (throw (Exception. "unexpected nucleotide character " base))))))


;; better version: tail recursion optimization, avoid possible stack overflow
(defn count_next_nucleotides [vec str]
  (if (empty? str)
    vec
    (let [base (first str)]
      (recur (condp = base
               \A [(inc (get vec 0)) (get vec 1) (get vec 2) (get vec 3)]
               \C [(get vec 0) (inc (get vec 1)) (get vec 2) (get vec 3)]
               \G [(get vec 0) (get vec 1) (inc (get vec 2)) (get vec 3)]
               \T [(get vec 0) (get vec 1) (get vec 2) (inc (get vec 3))]
               (throw (Exception. "unexpected nucleotide character " base)))
             (rest str)))))
           

;; (defn -main [& args]
;;   (let [str (rosalind.core/read_dataset (first args))
;;         vec (count_next_nucleotides [0 0 0 0] str)]
;;     (println (get vec 0) (get vec 1) (get vec 2) (get vec 3))))

(def-rosalind-main dna file
  (let [str (rosalind.core/read_dataset file)
        vec (count_next_nucleotides [0 0 0 0] str)]
    (println (get vec 0) (get vec 1) (get vec 2) (get vec 3))))
