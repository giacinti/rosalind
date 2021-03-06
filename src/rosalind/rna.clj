(ns rosalind.rna
  (:require [rosalind.core :refer [def-rosalind-main read-dataset]]))

(defn dna2rna [dna-str]
  (clojure.string/replace dna-str \T \U))

(defn -rosalind-main [file]
  (dna2rna (read-dataset file)))

(def-rosalind-main rna -rosalind-main)
