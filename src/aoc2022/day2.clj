(ns aoc2022.day2
  (:require [clojure.java.io :as io]))

(def mapping
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors})

(def rules
  {:rock     {:paper :lose :scissors :win}
   :paper    {:rock  :win  :scissors :lose }
   :scissors {:rock  :lose :paper :win}})

(def outcome
  {:win  6
   :draw 3
   :lose 0
   :rock     1
   :paper    2
   :scissors 3})

(def sample
  "A Y
   B X
   C Z")

(defn parse [txt]
  (->> txt
      clojure.string/split-lines
      (mapv (fn [x]
             (-> x clojure.string/trim (clojure.string/split #" "))))))


(defn score [[l r]]
  (let [them     (mapping l)
        you      (mapping r)
        result  (or (-> rules you them) :draw)]
    {:you you :them them
     :result result
     :score  (+ (outcome result) (outcome you))}))

(defn solve-2a []
  (->> (io/resource "day2input.txt")
       slurp
       parse
       (map (comp :score score))
       (reduce +)))

;;part two

(def target {"X" :lose
             "Y" :draw
             "Z" :win})

;;invert the rules based on their play.
;;win->lose, lose->win from their perspective.
(def results
  (let [invert {:win :lose
                :lose :win}]
    (reduce-kv (fn [acc them outcomes]
                 (assoc acc them
                        (into {} (for [[k v] outcomes] [(invert v) k]))))
               {} rules)))

(defn score2 [[l r]]
  (let [them     (mapping l)
        result   (target r)
        you      (or (-> results them result) them)] ;;draw is default
    {:you you :them them
     :result result
     :score  (+ (outcome result) (outcome you))}))

(defn solve-2b []
  (->> (io/resource "day2input.txt")
       slurp
       parse
       (map (comp :score score2))
       (reduce +)))
