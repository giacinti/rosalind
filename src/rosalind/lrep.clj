(ns rosalind.lrep
  (:require [rosalind.core :refer [def-rosalind-main dataset-seq]]))


(defn suffix-tree [strng inpt]
  (loop [line inpt
         tree {}
         parents {}]
    (if (empty? line)
      [tree parents]
      (let [[parent name sstart ssize]
            (clojure.string/split (first line) #"\s+")
            start (read-string sstart)
            size (read-string ssize)
            edge (subs strng (dec start) (dec (+ start size)))
            chld (get parents parent)]
        (recur (rest line)
               (assoc tree name (vector parent edge))
               (assoc parents parent (cons (vector name edge) chld)))))))

;; (defmacro def-node-depth [ptree]
;;   `(do
;;      (def node-depth#
;;        (memoize
;;         (fn [node-name#]
;;           (let [children# (map first (get ~ptree node-name#))]
;;             (if (empty? children#)
;;               1
;;               (reduce + 0 (doall (map node-depth# children#))))))))
;;      node-depth#))

(defmacro def-node-depth [ptree]
  `(with-local-vars
       [node-depth#
        (memoize
         (fn [node-name#]
           (let [children# (map first (get ~ptree node-name#))]
             (if (empty? children#)
               1
               (reduce + 0 (doall (map node-depth# children#)))))))]
     (.bindRoot node-depth# @node-depth#)
     @node-depth#))

;; (defmacro def-node-string [stree]
;;   `(do
;;      (def node-string#
;;        (memoize
;;         (fn [node-name#]
;;           (let [node# (get ~stree node-name# nil)]
;;             (if (nil? node#) ; this is the root
;;               ""
;;               (str (node-string# (first node#)) (second node#)))))))
;;      node-string#))

(defmacro def-node-string [stree]
  `(with-local-vars
       [node-string#
        (memoize
         (fn [node-name#]
           (let [node# (get ~stree node-name# nil)]
             (if (nil? node#) ; this is the root
               ""
               (str (node-string# (first node#)) (second node#))))))]
     (.bindRoot node-string# @node-string#)
     @node-string#))

(defn longest-repeated-substring [string depth
                                  child-suffix-tree parent-suffix-tree]
  (let [node-depth (def-node-depth parent-suffix-tree)
        node-string (def-node-string child-suffix-tree)
        cand (filter #(= (node-depth (key %)) depth) child-suffix-tree)]
    (apply max-key count (map #(node-string (key %)) cand))))

(defn -rosalind-main [file]
  (let [data (dataset-seq file)
        string (first data)
        depth (read-string (first (rest data)))
        [ctree ptree] (suffix-tree string (rest (rest data)))]
    (longest-repeated-substring string depth ctree ptree)))

(def-rosalind-main lrep -rosalind-main)
