(ns reducers.core-test
  (:require [reducers.core :refer :all]
            [primality.core :refer :all]))

;; Copyright Joe Einertson 2012.
;; Documentation by Henry Fellows

;; The first section of tests is just to figure out what things do.

;;Sanity check.
(+ 2 2)

;;So, this does not work.
;;Java throws a casting error.
;;  (primality.core/random)

;;mod-pow
;;It calculates arg1 to the power of arg2, then takes the modulo of arg3
;;(arg1 ^ arg2) mod arg3
(primality.core/mod-pow 2 3 9)

;;div-rem
;;".divideAndRemainder(BigInteger val) returns an array of two BigIntegers containing (this / val) followed by (this % val)."
;;It does something, but I'm not sure what.

(primality.core/div-rem 5 9)

;;

(rand-big-int 14325 1432567857)
