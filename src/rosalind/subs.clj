(ns rosalind.subs
  (require [rosalind.core :refer [def-rosalind-main dataset-seq]]))

(defmacro str-first [string]
  `(subs ~string 0 1))

(defmacro str-rest [string]
  `(subs ~string 1))

(defn sub-match? [segment dna]
  (= segment (subs dna 0 (count segment))))

(defn search-matches [solution segment index dna-str]
  (if (or (empty? dna-str) (< (count dna-str) (count segment)))
    solution
    (recur (if (sub-match? segment dna-str)
             (conj solution index)
             solution)
           segment
           (inc index)
           (str-rest dna-str))))

(defn -rosalind-main-old [file]
  (let [dataset (dataset-seq file)
        dna (first dataset)
        seq (first (rest dataset))]
    (search-matches [] seq 1 dna)))

;; thanks ogorman http://rosalind.info/users/ogorman/

(defn dna-partition [s m]
  ;stepwise partition of the input string 
  (map #(apply str %1) (partition (count m) 1 s)))

(defn dna-motif [s m]
  (apply str
         (interpose " " 
                    (remove nil?
                            (map-indexed #(if (= %2 m) (inc %1))
                                         (dna-partition s m))))))
(defn dna-motif [dna motif]
  (keep-indexed #(if (= %2 motif) (inc %1))
                (dna-partition dna motif)))

(defn -rosalind-main [file]
  (let [dataset (dataset-seq file)
        dna (first dataset)
        motif (first (rest dataset))]
    (apply str (interpose " " (dna-motif dna motif)))))
  
(def-rosalind-main subs -rosalind-main)

