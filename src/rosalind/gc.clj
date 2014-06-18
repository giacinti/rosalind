(ns rosalind.gc
  (:require [rosalind.core :refer [read-fasta-from-file def-rosalind-main]])
  (:require [rosalind.dna :refer [count-nucleotides]]))

(defn gc-content [dna]
  (let [r (count-nucleotides dna)
        gc (+ (r \G) (r \C))
        tot (+ gc (r \A) (r \T))]
    (* 100.0 (/ gc tot))))

(defn -rosalind-main [file]
  (let [mx (apply max-key #(get % 1)
                  (map #(vector (get % 0) (gc-content (get % 1)))
                       (rosalind.core/read-fasta-from-file file)))]
    (format "%s\n%.06f" (get mx 0) (get mx 1))))

(def-rosalind-main gc -rosalind-main)
