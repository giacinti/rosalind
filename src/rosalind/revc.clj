(ns rosalind.revc
  (:require [rosalind.core :refer [def-rosalind-main read-dataset]]))

;; (defn nucleotide_complement [base]
;;   (condp = base
;;     \A \T
;;     \C \G
;;     \G \C
;;     \T \A
;;     (throw (Exception. "unexpected nucleotide character " base))))

;; (defn reverse-complement [dna]
;;   (apply str (map nucleotide_complement
;;                   (clojure.contrib.string/reverse
;;                    dna))))

(def nucleotide_complement
  { 
   \A \T
   \C \G
   \G \C
   \T \A
   })

(defn reverse-complement [dna]
  (apply str (map nucleotide_complement
                  (clojure.string/reverse dna))))

(def-rosalind-main revc file
  (reverse-complement (read-dataset file)))
