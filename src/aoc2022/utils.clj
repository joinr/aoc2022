(ns aoc2022.utils)

(defn idx->xy [w h n]
  [(rem n w) (quot n h)])

(defn xy->idx [w h x y]
  (+ x (* y h)))
