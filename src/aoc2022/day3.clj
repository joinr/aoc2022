(ns aoc2022.day3
  (:require [clojure.java.io :as io]))

(def sample
"vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def priorities
  (->> (map vector (concat (range (int \a) (inc (int \z)))
                           (range (int \A) (inc (int \Z))))
            (range 1 53))
       (into {})))

(defn parse [txt]
  (->> txt
       clojure.string/split-lines
       (map (fn [x]
              (let [n (/ (count x) 2)]
                [(subs x 0 n) (subs x n (count x))])))
       (map (fn [[l r]]
              {:l l
               :r r
               :left  (into #{} (map int) l)
               :right (into #{} (map int r))}))))


(defn duplicates [{:keys [left right]}]
  (clojure.set/intersection left right))

(defn dupe-priorities [part]
  (let [dupes (duplicates part)]
    (assoc part
           :dupes dupes
           :priorities (mapv (fn [d]
                               [ d (priorities d) (char d)]) dupes))))

(defn total-priorities [parts]
  (->> parts
       (map dupe-priorities)
       (mapcat :priorities)
       (map (fn [[n p c]] p))
       (reduce +)))

(defn solve-3a []
  (->> (io/resource "day3input.txt")
       slurp
       parse
       total-priorities))

(defn badges [xs]
  (->> xs
       (partition 3)
       (map (fn [rucks]
              (let [common (->> rucks (map set) (apply clojure.set/intersection) first)]
                {:badge common
                 :priority (priorities (int common))}
                )))))

(defn solve-3b []
  (->> (io/resource "day3input.txt")
       slurp
       clojure.string/split-lines
       badges
       (map :priority)
       (reduce +)))
