(ns rosalind.rna
  (:require [rosalind.core :refer [def-rosalind-main read_dataset]]))

(defn dna2rna [dataset]
  (clojure.string/replace dataset \T \U))

(def-rosalind-main rna file
  (dna2rna (read_dataset file)))
