(ns rosalind.hamm
  (require [rosalind.core :refer [def-rosalind-main dataset-seq]]))

(defn hamming-distance [dna1 dna2]
  (apply + (map #(if (= %1 %2) 0 1) dna1 dna2)))

(defn -rosalind-main [file]
  (let [dataset (rosalind.core/dataset-seq file)
        dna1 (first dataset)
        dna2 (first (rest dataset))]
    (str (hamming-distance dna1 dna2))))

(def-rosalind-main hamm -rosalind-main)
