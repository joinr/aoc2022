(ns aoc2022.day1
  (:require [clojure.java.io :as io]))

(def sample
  "1000
   2000
   3000

   4000

   5000
   6000

   7000
   8000
   9000

   10000")

(defn parse [txt]
  (->> txt
       (clojure.string/split-lines)
       (map clojure.string/trim)
       (partition-by clojure.string/blank?)
       (filter #(not= (first %) ""))
       (map-indexed (fn [idx cals]
                      (let [cals (mapv parse-long cals)]
                        {:elf idx :total (apply + cals) :cals cals})))))

(defn largest [xs]
  (->> xs
       (map :total)
       (reduce max)))

(defn solve-1a []
  (->> (io/resource "day1input.txt")
       slurp
       parse
       largest))

(defn top-3 [xs]
  (->> xs
       (sort-by (comp - :total))
       (take 3)))

(defn solve-1b []
  (->> (io/resource "day1input.txt")
       slurp
       parse
       top-3
       (map :total)
       (reduce +)))
