(ns aoc2022.day8
  (:require [clojure.java.io :as io]))

(def sample
"30373
25512
65332
33549
35390")

;;visibility query.
;;given:
;;a set of nodes that are visible.
;;unknown nodes (fringe).
;;can we find a path from current
;;node to a visible node?


(defn parse [txt]
  (->> txt
       (clojure.string/split-lines)
       (mapv #(mapv (comp parse-long str) %))))

(defn scanl [row]
  (let [bound (count row)]
    (loop [idx       0
           highest  -1
           acc   #{}]
      (if (< idx bound)
        ;;keep scanning
        (let [nxt  (row idx)
              nidx (unchecked-inc idx)]
          (if (< highest nxt) ;;visible.
            (recur nidx nxt (conj acc idx))
            (recur nidx highest acc)))
        acc))))

(defn scanr [row]
  (loop [idx (dec (count row))
         highest  -1
         acc   #{}]
    (if (> idx -1)
      ;;keep scanning
      (let [nxt  (row idx)
            nidx (unchecked-dec idx)]
        (if (< highest nxt) ;;visible.
          (recur nidx nxt (conj acc idx))
          (recur nidx highest acc)))
      acc)))

(defn visibles [row]
  (into (scanl row) (scanr row)))

(defn transpose [m]
  (let [idxs (range (count (first m)))]
  (reduce (fn [acc row]
            (reduce (fn [acc idx]
                      (update acc idx conj (row idx))) acc idxs))
          (vec (repeat (count (first m)) [])) m)))

(defn visindices [rows]
  (let [cols (transpose rows)]
    (->> (concat (for [i (range (count rows))
                       j (visibles (rows i))]
                   [i j])
                 (for [j (range (count cols))
                       i (visibles (cols j))]
                   [i j]))
         set)))

(defn view [rows results]
  (let [M (dec (count rows))
        N (dec (count (first rows)))
        noted (reduce (fn [acc [i j :as v]]
                        (update-in acc [i j]
                                   (fn [n]
                                     (cond (or (= i 0) (= j 0) (= i M) (= j N))
                                       (str "|" n "|")
                                       (results v)
                                       (str "<" n ">")
                                       :else
                                       (str "_" n "_")))))
                        rows (for [i (range (count rows))
                                   j (range (count (first rows)))]
                               [ i j]))]
    (doseq [row noted]
      (println row))))

(defn solve-8a []
  (->> (io/resource "day8input.txt")
       slurp
       parse
       visindices
       count))

;;part 2
;;based on scoring criterion, we observe that the best possible
;;for any theortical mapping, would be a max value in the middle surrounded
;;by lower values s.t. the view in each direction is unimpeded.
;;if we know the grid is m x n, and the dimensions are odd (they are),
;;and it's square (it is).
;;the the optimal point is ((n-1)/2 + 1), ((n-1)/2 + 1) if there is an unimpeded view.
;;where ((n-1)/2 + 1) is k, thus (k,k) is the center.
;;e.g. max tree counts in all directions, gives us
;;(k-1)*(k-1)*(k-1)*(k-1)
;;2*2*2*2 = 8. 8 is the best we can do for n = 8.

;;We also realize that the next best set of points with equal
;;potential view values are the 4 neighbors of the optimum.
;;Since they are just a mixture of products, where we have decreased
;;one dimension and increased another, we have a constant value for all of them.
;;e.g. for the left neighbor of (k,k), (k-1,k), we have:
;;l        u    r d
;;(k-2)*(k-1)*k*(k-1)
;;1*2*3*2 = 12.  This will hold for all children of k.
;;The holds for the children of the children of k.  They will
;;have the same value.

;;So we have expanding rings of candidates that we can explore in order
;;to exhaustively find the best vantage in a breadth first fashion.

(defn directions [x y]
  {:left  [(dec x)  y]
   :up    [x       (inc y)]
   :right [(inc x)  y]
   :down  [x       (dec y)]})

(defn breadth-neighbors [from x y]
  (dissoc (directions x y) from))

;; (defn optimal-order [rows]
;;   (let [m (count rows)
;;         n (count (first rows))]
;;     (iterate (fn [xs]
;;                (for [[from [x y]] xs]
;;                  (case from
;;                    :left
;;                    :up
;;                    :right
;;                    :down  ))

