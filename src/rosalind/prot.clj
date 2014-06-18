(ns rosalind.prot
  (:require [rosalind.core :refer [def-rosalind-main dataset-seq RNA-codon-table]]))

(defmacro str-head [str n]
  `(let [s# ~str]
     (if (< (count s#) ~n)
       s#
       (.substring s# 0 ~n))))

(defmacro str-rest [str n]
  `(let [s# ~str]
     (if (< (count s#) ~n)
       s#
       (.substring s# ~n))))


(defn encoded-protein-string [rna-string]
  (loop [prot ""
         rna rna-string]
    (if (or (empty? rna) (< (count rna) 3))
      prot
      (recur (str prot (get RNA-codon-table (str-head rna 3)))
             (str-rest rna 3)))))
          

(defn -rosalind-main [file]
  (encoded-protein-string (first (dataset-seq file))))

(def-rosalind-main prot -rosalind-main)
