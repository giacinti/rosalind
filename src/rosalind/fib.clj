(ns rosalind.fib
  (require [rosalind.core :refer [def-rosalind-main read-vector-from-file]]))

(defn make-rosalind-fibo [children-pairs-nb start]
  (with-local-vars
      [fib (memoize
            (fn [n]
              (if (<= n 2)
                start
                (+ (* children-pairs-nb (fib (- n 2)))
                   (fib (dec n))))))]
    (.bindRoot fib @fib)
    @fib))

(defn -rosalind-main [file]
  (let [dataset (read-vector-from-file file)
        months (first dataset)
        nchild (last dataset)]
    (let [f (make-rosalind-fibo nchild 1)]
      (str (f months)))))

(def-rosalind-main fib -rosalind-main)
