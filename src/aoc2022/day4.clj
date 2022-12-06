(ns aoc2022.day4
  (:require [clojure.java.io :as io]))

(def sample
"2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(def splits
  {"-" #"-"
   "," #","})
(defn ->longs [xs] (mapv parse-long xs))

(defn split-on [x char]
  (clojure.string/split x (splits char)))

(defn parse [txt]
  (->> txt
       clojure.string/split-lines
       (map #(split-on  % ","))
       (map (fn [[l r]]
              [(->longs (split-on l "-"))
               (->longs (split-on r "-"))]))))

;;intervals arith.
;;l is contained by r.
(defn inside?  [[l1 r1] [l2 r2]]
  (and (>= l1 l2)
       (<= r1 r2)))

(defn between? [x l r]
  (<= l x r))

(defn intersects? [[l1 r1] [l2 r2]]
  (or (between? l1 l2 r2)
      (between? r1 l2 r2)
      (between? l2 l1 r1)
      (between? r2 l1 r1)))

(defn either-contains? [l r]
  (cond (inside? l r)
        {:outer r
         :inner l}
        (inside? r l)
        {:outer r  :inner l}
        :else nil))

(defn solve-4a []
  (->> (io/resource "day4input.txt")
       slurp
       parse
       (map (fn [[l r]] (either-contains? l r)))
       (filter identity)
       count))

(defn solve-4b []
  (->> (io/resource "day4input.txt")
       slurp
       parse
       (map (fn [[l r]] (intersects? l r)))
       (filter identity)
       count))
