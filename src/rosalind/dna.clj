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

;; (def-rosalind-main dnaV1 file
;;   (let [str (rosalind.core/read_dataset file)
;;         vec (count_next_nucleotides [0 0 0 0] str)]
;;     (println (get vec 0) (get vec 1) (get vec 2) (get vec 3))))

(defn -count-one-nucleotide [base] 
  (condp = base
    \A { \A 1, \C 0, \G 0, \T 0 }
    \C { \A 0, \C 1, \G 0, \T 0 }
    \G { \A 0, \C 0, \G 1, \T 0 }
    \T { \A 0, \C 0, \G 0, \T 1 }
    (throw (Exception. "unexpected nucleotide character " base))))

(defn -sum-dna-hash [h1 h2]
  { \A (+ (h1 \A) (h2 \A)),
    \C (+ (h1 \C) (h2 \C)),
    \G (+ (h1 \G) (h2 \G)),
    \T (+ (h1 \T) (h2 \T)) })

(defn count-nucleotides [dna-str]
  (reduce -sum-dna-hash
          (map -count-one-nucleotide
               dna-str)))

(def-rosalind-main dna file
  (count-nucleotides (rosalind.core/read-dataset file)))


