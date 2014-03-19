(ns primality.core
  (:require [clojure.math.numeric-tower :as math])
  (:import [java.math BigInteger]
           [java.util Random])
  (:gen-class
    :name com.joe.primality
    :methods [#^{:static true} [fermatTest [Number int] boolean]
              #^{:static true} [randBigInt [Number Number] java.math.BigInteger]
              #^{:static true} [findFactors [Number] clojure.lang.PersistentVector]]))

;;So, this does not work.
;;Java throws a casting error.
(def random (Random.))

;;mod-pow
;;Takes 3 ints.
;;It puts arg1 to the power of arg2, then takes the modulo of arg3
;;(arg1 ^ arg2) mod arg3
(defn mod-pow [a b c]
  (.modPow (biginteger a) (biginteger b) (biginteger c)))

;;mod-pow
;;It calculates arg1 to the power of arg2, then takes the modulo of arg3
;;(arg1 ^ arg2) mod arg3
(defn div-rem [a b]
  (.divideAndRemainder (biginteger a) (biginteger b)))

(defn rand-big-int [low high]
  (let [num-bits (.bitLength (biginteger high))]
    (some #(when (<= low % high) %) (repeatedly #(BigInteger. num-bits random)))))

(defn fermat-test [n k]
  (nil? (some false? (for [x (range k)]
                       (let [nd (-' n 1)
                             a (rand-big-int 1 nd)
                             r (mod-pow a nd n)]
                         (= r 1))))))

(defn find-factors [p]
  (loop [u 1]
    (let [z (math/expt 2 u)
          [r remainder] (div-rem p z)]
    (cond
      (> z p) nil
      (and (= 0 remainder) (odd? r)) [u r]
      :else (recur (inc u))))))

; Java-friendly names for exported methods
(def -fermatTest fermat-test)
(def -randBigInt rand-big-int)
(def -findFactors find-factors)
