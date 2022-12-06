(ns aoc2022.day6
  (:require [clojure.java.io :as io]))

(def sample "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

;;we could use partition and check them all.
;;or just scan with a buffer.
;;could also try binary searching by partitions.
;;naive solution is just scan...

;;if we trade a character from the buffer.
;;we only have 2 checks.
;;The outgoing char decrements the knon chars.
;;the incoming incs the new char.
;;If the outgoing decrements the knowns to 1,
;;and the incoming keeps the knowns at 1, then we have no dupes.
;;can use ring buffer.

;;want a bag.  model with a map.
(defn bconj [b k]
  (update b k (fn [n] (inc (or n 0)))))
(defn bdisj [b k]
  (if-let [v (b k)]
    (if (= v 1)
      (dissoc b k)
      (update b k dec))))

(defn ->bag [xs]
  (reduce bconj {} xs))

(defn scan
  ([known n left idx txt]
  ;;block scan by keeping track of distinct chars, and
  ;;the idx we are on.
   ;;If the number of distinct chars = n, we are done.
   #_(println [known n  left idx (subs txt (- idx n) idx)])
   (if
     (= (count known) n)
     {:idx idx :txt (subs txt (- idx n) idx)}
     (let [nxt      (unchecked-inc idx)
           right    (nth txt idx)
           new-left (nth txt (- nxt n))]
       (recur (-> known (bdisj left) (bconj right)) n  new-left nxt txt))))
  ([n txt] (scan (->bag (take n txt)) n (nth txt 0) n  txt)))

(defn solve-6a []
  (->> (io/resource "day6input.txt")
       slurp
       (scan 4)))

(defn solve-6b []
  (->> (io/resource "day6input.txt")
       slurp
       (scan 14)))
