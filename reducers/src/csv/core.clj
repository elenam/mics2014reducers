;; Henry Fellows, based on work by Joe Einertson.
;; fello056@morris.umn.edu and hfellows on Github.
;; Licensed under the creative commons share-alike 3.0 license.

(ns csv.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

;; Joe's data comes in a non-friendly format. For example;
;;-----------------
;; ({:fn-name {:result val, :time val2}, :fn-name2 {:result val, :time val2}, :fn-name3 {:result val, :time val2}}
;;  {:fn-name {:result val, :time val2}, :fn-name2 {:result val, :time val2}, :fn-name3 {:result val, :time val2}}
;;  {:fn-name {:result val, :time val2}, :fn-name2 {:result val, :time val2}, :fn-name3 {:result val, :time val2}})
;;-----------------
