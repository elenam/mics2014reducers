(ns reducers.core
  (:require [clojure.core.reducers :as r]
            [clojure.string :as string]
            [clojure.math.numeric-tower :as cljmath]
            [primality.core :as p]
            [reducers-printing.core :as rep])
  (:import [java.lang Math]
           [java.util Random]))


(def pattern #"(x+x)+y")

(defn wtfredfn [a v]
  (println "[" a v "]")
  (let [c (if (string? a) (count (re-matches pattern a)) a)]
    (max c (count (re-matches pattern v)))))

;(def mapfn #(p/fermat-test % 5))
;(def reducefn (r/monoid #(do (Thread/sleep 10) (and %1 %2)) #(do false)))
;(def reducefn (r/monoid #(some true? [%1 %2]) #(do false)))
;(def mapfn #(str % %))
;(def reducefn (r/monoid wtfredfn #(do 3)))
(def mapfn #(cljmath/sqrt %))
(def reducefn (r/monoid + #(do 0)))



;(132 66 44 33 132/5 22 132/7 33/2 44/3 66/5)

(defn prime-factor? [f n]
  (let [q (/ n f)]
  ;(and
    (integer? q)))
    ;(p/fermat-test f 5)))

(defn factor [x]
  (let [[k _] (cljmath/exact-integer-sqrt x)]
    (println k)
    (filter #(prime-factor? % x) (range 1 (inc k)))))

(defn vfold
  ([f coll]
     (let [cores (.. Runtime getRuntime availableProcessors)
           workers (int (Math/floor (* 3/4 cores)))
           base (* 8 cores)
           n (max 2 (int (Math/floor (/ (count coll) (* 2 workers)))))
           n (min base n)]
       #_(println :>>N n)
       (vfold f n coll)))
  ([f n coll]
     (if (< n 1)
       (vfold f coll)
       (r/fold n
               (fn
                 ([] [])
                 ([l r] (apply conj l r)))
               (fn[v x] (conj v (f x)))
               (vec coll)))))

(defn rand-seq [n max-val]
  (repeatedly n #(p/rand-big-int 1 max-val)))

(defn transform [data reducer mapper]
  (reducer reducefn (mapper mapfn data)))

(defn run-reducers [data]
  (transform data r/reduce r/map))

(defn run-preducers [data]
  (transform data r/fold r/map))

(defn run-core [data]
  (transform data reduce map))

(defn run-pcore [data]
  (transform data reduce pmap))

(defn run-vfold [data]
  (transform data vfold pmap))

(defn str-time
  ([f] (str-time f []))
  ([f args]
  (let [zzz (println "timing")
        start (System/currentTimeMillis)
        result (apply f args)]
    {:value result, :time (- (System/currentTimeMillis) start)})))

(defn rand-int-data
  ([n] (rand-int-data n 1000000000))
  ([n max-size] (rand-seq n max-size)))

(defn rand-strs [n max-len]
  (repeatedly n
    #(apply str (take (rand-int max-len) (repeat "X")))))

(defn bad-str-data [n]
  (let [hn (int (/ n 2))]
    (repeatedly n (fn []
                     (let [z (+ hn (rand-int hn))]
                       (string/join (concat (repeat z "x") ["y"])))))))

(defn time-runs [n coll-size testfns datafn]
  (for [x (range n)
    :let [data (doall (datafn coll-size))]]
    (map #(str-time %1 [data]) testfns)))

(defn reduce-str-len
  ([] -1)
  ([a b] (max a (count b))))

(defn compare-str-len [runs strs str-size]
  (for [x (range runs)
    :let [data (doall (rand-strs strs str-size))]]
    {:map     (str-time #(reduce max (map count data)))
     :pmap    (str-time #(reduce max (pmap count data)))
     :preduce (str-time #(r/fold reduce-str-len data))}))

(defn reduce-sum-primes
  ([] 0)
  ([a b] (if (p/fermat-test b 5)
          (+' a b)
          a)))

(defn compare-sum-primes [runs nums num-size]
  (for [x (range (inc runs))
    :let [data (vec (doall (rand-int-data nums num-size)))
          mf #(if (p/fermat-test % 5) % 0)]]
    {:reduce-and-map       (str-time (fn [] (reduce +' (map mf data))))
     :reduce-and-pmap      (str-time (fn [] (reduce +' (pmap mf data))))
     :r/fold-and-pmap      (str-time (fn [] (r/fold +' (pmap mf data))))
     :r/fold-and-map       (str-time (fn [] (r/fold +' (map mf data))))
     :r/fold-and-r/map     (str-time (fn [] (r/fold +' (r/map mf data))))
     :r/fold               (str-time (fn [] (r/fold reduce-sum-primes data)))
     }))

(defn reduce-sum-sqrt
  ([] 0)
  ([a b] (+' a (cljmath/sqrt b))))

(defn compare-sum-sqrt [runs nums num-size]
  (for [x (range (inc runs))
    :let [data (vec (doall (rand-int-data nums num-size)))
          mf cljmath/sqrt]]
    {:reduce             (str-time (fn [] (reduce reduce-sum-sqrt data)))
     :reduce-and-map     (str-time (fn [] (reduce +' (map mf data))))
     :reduce-and-pmap    (str-time (fn [] (reduce +' (pmap mf data))))
     :r/fold-and-pmap    (str-time (fn [] (r/fold +' (pmap mf data))))
     :r/fold-and-r/map   (str-time (fn [] (r/fold +' (r/map mf data))))
     :r/fold             (str-time (fn [] (r/fold reduce-sum-sqrt data)))}))

;;;Black Magic
(defn reduce-count-sqrt
  ([] 0)
  ([a b] (if (p/fermat-test b 5) (inc b) b)))

(defn compare-count-prime [runs nums num-size]
  (for [x (range (inc runs))
    :let [data (vec (doall (rand-int-data nums num-size)))
          mf #(if (p/fermat-test % 5) 1 0)]]
    {:reduce-and-map     (str-time (fn [] (reduce +' (map mf data))))
     :reduce-and-pmap    (str-time (fn [] (reduce +' (pmap mf data))))
     :r/fold-and-pmap    (str-time (fn [] (r/fold +' (pmap mf data))))
     :r/fold-and-map     (str-time (fn [] (r/fold +' (map mf data))))
     :r/fold-and-r/map   (str-time (fn [] (r/fold +' (r/map mf data))))
     :r/fold             (str-time (fn [] (r/fold reduce-count-sqrt data)))}))


;;;;;;;;;;;;;;;;;;;;;
;;;;Begin Comment;;;;
;;;;;;;;;;;;;;;;;;;;;
(comment

(defn reduce-num-primes
  ([] 0)
  ([a b] (if (p/fermat-test b 5)
          (+' 1 a)
          a)))

(defn compare-num-primes [runs nums num-size]
  (for [x (range runs)
    :let [data (doall (rand-int-data nums num-size))
          mf #(if (p/fermat-test % 5) 1 0)]]
    {:reduce-and-map     (str-time (fn [] (reduce +' (map mf data))))
     :reduce-and-pmap    (str-time (fn [] (reduce +' (pmap mf data))))
     :r/fold-and-pmap    (str-time (fn [] (r/fold +' (pmap mf data))))
     :r/fold-and-map     (str-time (fn [] (r/fold +' (map mf data))))
     :r/fold             (str-time (fn [] (r/fold reduce-num-primes data)))}))

(defn compare-sum-primes-same [runs nums num-size]
  (let [data (doall (rand-int-data nums num-size))
        mf #(if (p/fermat-test % 5) % 0)]
  (for [x (range runs)]
    {:reduce-and-map      (str-time (fn [] (reduce +' (map mf data))))
     :reduce-and-pmap     (str-time (fn [] (reduce +' (pmap mf data))))
     :r/fold-and-pmap     (str-time (fn [] (r/fold +' (pmap mf data))))
     :r/fold-and-map      (str-time (fn [] (r/fold +' (map mf data))))
     :r/fold              (str-time (fn [] (r/fold reduce-sum-primes data)))})))

(defn reduce-expensive-map
  ([] 0)
  ([a b] (+' a (cljmath/sqrt (cljmath/expt b 3)))))

(defn compare-expensive-map [runs nums num-size]
  (for [x (range (inc runs))
    :let [data (vec (doall (rand-int-data nums num-size)))
          mf #(cljmath/sqrt (cljmath/expt % 3))]]
    {:map      (str-time (fn [] (reduce +' (map mf data))))
     :pmap     (str-time (fn [] (reduce +' (pmap mf data))))
     :pall     (str-time (fn [] (r/fold +' (pmap mf data))))
     :preduce  (str-time (fn [] (r/fold reduce-expensive-map data)))}))

(defn compare-max [runs nums num-size]
  (for [x (range runs)
    :let [data (vec (doall (rand-int-data nums num-size)))]]
    (do
      (println "Starting round " x)
      {:reduce  (str-time (fn [] (reduce max data)))
       :preduce (str-time (fn [] (r/fold (r/monoid max #(do -1)) data)))})))


(defn compare-sum [runs nums num-size]
  (for [x (range (inc runs))
    :let [data (vec (doall (rand-int-data nums num-size)))]]
    {:reduce  (str-time (fn [] (reduce +' data)))
     :preduce (str-time (fn [] (r/fold +' data)))}))

(defn compare-product [runs nums num-size]
  (for [x (range (inc runs))
    :let [data (vec (doall (rand-int-data nums num-size)))]]
    (do (println "Starting round " x)
      {:reduce  (str-time (fn [] (reduce *' data)))
       :preduce (str-time (fn [] (r/fold *' (r/map #(do %) data))))})))

(defn compare-inc-sum [runs nums num-size]
  (for [x (range (inc runs))
    :let [data (doall (rand-int-data nums num-size))]]
    (do (println "Starting round " x)
      {:reduce  (str-time (fn [] (reduce +' (map inc (map inc (map inc data))))))
       :preduce (str-time (fn [] (r/fold +' (r/map inc (r/map inc (r/map inc data))))))})))
)
;;;;;;;;;;;;;;;;;;;
;;;;End Comment;;;;
;;;;;;;;;;;;;;;;;;;

(defn print-with-gap [coll]
  (doall (map #(println % "\n\n\n") coll)))

(defn print-for-term [coll]
  (doall (map (fn [round]
                (println
                  (string/join "	"
                               (map (fn [result] (str (first result) " " (:time (second result)))) round)))) (rest coll))))

(defn -main [& args]
  (println "started")
  ;(print-for-term (compare-sum 10 1000000 1000000000)) ; <<< Ganesha 1
  ;(print-for-term (compare-max 10 1000000 1000000000)) ; <<< Ganesha 2
  (rep/csv-print (compare-sum-sqrt 1000 100000 1000000000)) ; <<< Ganesha 3
  ;(rep/csv-print (compare-count-prime 100 100000 1000000000)) ; <<< Ganesha 4
  ;(rep/csv-print (compare-sum-primes 1000 10000 1000000000))
  (System/exit 0)
  (let [coll-size 500000 ;1000000
        testfns [run-reducers run-preducers]; run-core run-pcore]
        num-runs 10]
    (println "running tests")
    (print-with-gap (time-runs num-runs coll-size testfns rand-int-data))
    (println "done"))
  (System/exit 0))
