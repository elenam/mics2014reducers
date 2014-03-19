(ns reducers-printing.core
  (:require [clojure.string :as string]))

;; Henry Fellows, based on work by Joe Einertson.
;; fello056@morris.umn.edu and hfellows on Github.
;; Licensed under the creative commons share-alike 3.0 license.

;;;; REDUCERS-PRINTING
;;;  This is a library for pretty-printing from Joe Einertson's timing system
;;;  It is largely focused on printing to csv.

;; A note on CSV.
;; I'm going to use the idiomatic way of storing data; columns for each category.
;; Rows are more natural for a list of things (in this case, times), but idiom is important.

;;;; CSV PRINTING
;;;  A pile of functions ment to print timing data in .csv

; Joe's data comes in a non-friendly format. For example;
;-----------------
; ({:fn-name {:value val1, :time val2}, :fn-name2 {:value val1, :time val2}}
;  {:fn-name {:value val1, :time val2}, :fn-name2 {:value val1, :time val2}}
;  {:fn-name {:value val1, :time val2}, :fn-name2 {:value val1, :time val2}})
;-----------------
; I've named the structure a <run>, which contains <rounds>, which contain <results>, and then the data is <time>
; :value is the result of the function being timed.

; Here's an example of what the output should be:
;-----------------
; "fn-name, fn-name, fn-name <newline>
;  value, value, value <newline>
;  value, value, value <newline>
;  value, value, value <newline>"

;;Get the keys from a collection of maps (that each have the same key-values)
(defn get-names
  "Returns a collection of keys given a map in a collection."
  [a-run]
  (str (string/join ", " (keys (first a-run))) "\r\n"))

;;Get :time from a map
(defn get-time
  "Applies the key :time on the map given."
  [a-map]
  (:time a-map))


;;Get the time values from each map in a map.
; A round is a map of maps
(defn get-round
  "Applies get-time to a map of maps."
  [a-round]
  (map get-time (vals a-round)))


;;Get all of the time values from a collection of maps of maps
; A run is a collection of maps of maps
(defn get-run
  "Applies get-round to a collection of maps of maps."
  [a-run]
  (map get-round a-run))


;; make it into a string
(defn re-str
  "Recursively builds a string from the .toString of each item in a collection of collections"
  [times]
  (if (empty? times) "" (string/join "\r\n" [(string/join ", " (first times)) (re-str (rest times))])))

;; add the header
(defn run->csv
  "concatenates the results of running get-names and re-str on a run of Joe Einertson's timing system"
  [a-run]
  (str (get-names a-run)
       (re-str (get-run a-run))))

;;;Sanity Testing of the above

(def run '({:smap {:result 525, :time 592}, :pmap {:result 525, :time 131}, :pall {:result 525, :time 98}, :preduce {:result 775458338N, :time 79}}
           {:smap {:result 530, :time 220}, :pmap {:result 530, :time 71}, :pall {:result 530, :time 72}, :preduce {:result 190658262, :time 60}}
           {:smap {:result 533, :time 217}, :pmap {:result 533, :time 71}, :pall {:result 533, :time 64}, :preduce {:result 433484551, :time 65}}
               ))

(def round {:smap {:result 525, :time 592}, :pmap {:result 525, :time 131}, :pall {:result 525, :time 98}, :preduce {:result 775458338N, :time 79}})

(def result {:result 525, :time 592})

(get-names run)

(get-time result)

(get-round round)

(get-run run)

(re-str (get-run run))

(run->csv run)

;; append it to a file
(defn csv-print [a-run] (spit "data.csv" (run->csv a-run)))


