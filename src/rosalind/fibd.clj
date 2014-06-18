(ns rosalind.fibd
  (require [rosalind.core :refer [def-rosalind-main read-vector-from-file]]))

(defn make-rosalind-fibd [children-pairs-nb start longevity]
  (with-local-vars
      [fib (memoize
            (fn [n]
              (cond
                (< n 0) 0
                (and (>= n 0) (<= n 2)) start
                :else (- (+ (* children-pairs-nb (fib (- n 2)))
                            (fib (dec n)))
                         (fib (dec (- n longevity))))
                )))]
    (.bindRoot fib @fib)
    @fib))

(defn -rosalind-main [file]
  (let [dataset (read-vector-from-file file)
        months (first dataset)
        longv (last dataset)]
    (let [f (make-rosalind-fibd 1 1 longv)]
      (str (f months)))))

(def-rosalind-main fibd -rosalind-main)
