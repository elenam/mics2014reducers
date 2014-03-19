(ns sorting.core)

(defn insert [n lst]
  (cond 
    (empty? lst) (list n)
    (> (first lst) n) (conj lst n)
    :else (conj (insert n (rest lst)) (first lst))))

(defn insertion-sort [lst]
  (loop [list lst result '()]
    (if (empty? list) result
        (recur (rest list) (insert (first list) result)))))
