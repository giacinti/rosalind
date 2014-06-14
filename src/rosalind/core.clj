(ns rosalind.core
  (:use [clojure.java.io])
  (:require [clojure.string :refer [trim-newline]]))

(import '(java.io BufferedReader StringReader))

(def infinity (/ 1.0 0.0))

(def RNA-codon-table
  {
   "UUU" "F"
   "CUU" "L"
   "AUU" "I"
   "GUU" "V"
   "UUC" "F"
   "CUC" "L"
   "AUC" "I"
   "GUC" "V"
   "UUA" "L"
   "CUA" "L"
   "AUA" "I"
   "GUA" "V"
   "UUG" "L"
   "CUG" "L"
   "AUG" "M"
   "GUG" "V"
   "UCU" "S"
   "CCU" "P"
   "ACU" "T"
   "GCU" "A"
   "UCC" "S"
   "CCC" "P"
   "ACC" "T"
   "GCC" "A"
   "UCA" "S"
   "CCA" "P"
   "ACA" "T"
   "GCA" "A"
   "UCG" "S"
   "CCG" "P"
   "ACG" "T"
   "GCG" "A"
   "UAU" "Y"
   "CAU" "H"
   "AAU" "N"
   "GAU" "D"
   "UAC" "Y"
   "CAC" "H"
   "AAC" "N"
   "GAC" "D"
   "UAA" ""
   "CAA" "Q"
   "AAA" "K"
   "GAA" "E"
   "UAG" ""
   "CAG" "Q"
   "AAG" "K"
   "GAG" "E"
   "UGU" "C"
   "CGU" "R"
   "AGU" "S"
   "GGU" "G"
   "UGC" "C"
   "CGC" "R"
   "AGC" "S"
   "GGC" "G"
   "UGA" ""
   "CGA" "R"
   "AGA" "R"
   "GGA" "G"
   "UGG" "W"
   "CGG" "R"
   "AGG" "R"
   "GGG" "G" 
   })

(def monoisotopic-mass-table
  {
   \A 71.03711
   \C 103.00919
   \D 115.02694
   \E 129.04259
   \F 147.06841
   \G 57.02146
   \H 137.05891
   \I 113.08406
   \K 128.09496
   \L 113.08406
   \M 131.04049
   \N 114.04293
   \P 97.05276
   \Q 128.05858
   \R 156.10111
   \S 87.03203
   \T 101.04768
   \V 99.06841
   \W 186.07931
   \Y 163.06333 
   })

(def start-codon "AUG")

(defn start-codon? [str]
  (= str start-codon))

(defn stop-codon? [str]
  (or (= str "UAA")
      (= str "UAG")
      (= str "UGA")))

(defn read_dataset [filename]
  (trim-newline (slurp filename)))

(defn dataset-seq [filename]
  (line-seq (clojure.java.io/reader filename)))

(defn read-fasta [rdr]
  (apply hash-map
         (map #(if (.startsWith ^String % ">")
                 (.substring % 1)
                 %)
              (map #(apply str %)
                   (partition-by #(.startsWith ^String % ">")
                                 (line-seq rdr))))))

(defn read-fasta-from-file [filename]
  (read-fasta (reader filename)))

(defn read-fasta-from-string [strng]
  (read-fasta (BufferedReader. (StringReader. strng))))

(defn read-vector-from-stream [stream]
  (loop [v (read stream false nil)
         result []]
    (if (nil? v)
      result
      (recur (read stream false nil) (conj result v)))))

(defn read-vector-from-file [filename]
  (with-open
      [stream (java.io.PushbackReader.
               (clojure.java.io/reader filename))]
    (read-vector-from-stream stream)))

(defn read-vector-from-string [string]
  (with-in-str string
    (read-vector-from-stream *in*)))

(defmacro abs [expr]
  `(let [v# ~expr]
     (if (< v# 0) (- 0 v#) v#)))

(defn in? 
  "true if seq contains elm"
  [seq elm]  
  (some #(= elm %) seq))

(defmacro def-rosalind-main [name file & body]
  `(let [sample# (clojure.java.io/resource (str '~name ".sample.txt"))
         dataset# (clojure.java.io/resource (str '~name ".dataset.txt"))]

     (defn ~name [~file] ~@body)
     (defn ~'-sample [] (~name sample#))
     (defn ~'-dataset [] (~name dataset#))
     (defn ~'-main [& args#]
       (loop [a# args#]
         (if (not (empty? a#))
           (let [p# (first a#)]
             (cond
              (= p# "sample") (~'-sample)
              (or (= p# "dataset") (= p# "data")) (~'-dataset)
              :else (println "don't know what to do with " p#))
             (recur (rest a#))))))))
