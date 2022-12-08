(ns aoc2022.day8
  (:require [clojure.java.io :as io]
            [aoc2022.utils :as u]))

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

(defn assoc-some [m k v]
  (if v
    (assoc m k v)
    m))

(defn directions [xmin xmax ymin ymax x y]
  (-> {}
      (assoc-some :left  (when (> x xmin) [(dec x)  y]))
      (assoc-some :up    (when (< y ymax) [x       (inc y)]))
      (assoc-some :right (when (< x xmax) [(inc x)  y]))
      (assoc-some :down  (when (> y ymin) [x       (dec y)]))))

(defn breadth-neighbors [from xmin xmax ymin ymax x y]
  (dissoc (directions xmin xmax ymin ymax x y) ({:left :right
                                                 :up :down} from)))

(defn optimal-order [rows]
  (let [n (count rows)
        xmin 0
        xmax (dec n)
        ymin 0
        ymax (dec n)
        k    (+ xmin (quot n 2))
        as-idx   (fn [x y] (u/xy->idx n n x y ))
        as-xy    (fn [idx] (u/idx->xy n n idx))
        known    (atom #{(as-idx k k)})
        open?    (fn [x y]
                   (let [idx (as-idx x y)]
                     (when (not (@known idx))
                       (do (swap! known conj idx)
                           true))))
       ]
    (->> (iterate (fn [xs]
                    (apply concat
                           (for [[from [x y]] xs]
                             (do (swap! known conj (as-idx x y))
                                 (->> (breadth-neighbors from xmin xmax ymin ymax x y)
                                      (filter (fn [[_ [x y]]] (open? x y))))))))
                  [[:middle [k k]]])
         (take-while seq)
         (apply concat)
         (map second))))

(defn view-order [rows xs]
  (reduce (fn [acc [idx [x y]]]
            (assoc-in acc [x y] idx))
          rows
          (map-indexed vector xs)))

(comment
[22 14  5 13  21]
[16  7  1  6  15]
[12  4  0  2   8]
[20 11  3  9  17]
[24 19 10 18  23])

;;now need to define linear scan for visibility from [x y]...

(defn horizontal [xmin xmax x]
  [(range (inc x) (inc xmax))
   (range (dec x) (dec xmin) -1)])

(defn vertical   [ymin ymax y]
  [(range (inc y) (inc ymax))
   (range (dec y) (dec ymin) -1)])

(defn scorex [xy->v v y xs]
  (reduce (fn [acc x]
            (let [nxt (unchecked-inc acc)]
              (if (< (xy->v x y) v)
                nxt
                (reduced nxt))))
          0 xs))

(defn scorey [xy->v v x ys]
  (reduce (fn [acc y]
            (let [nxt (unchecked-inc acc)]
              (if (< (xy->v x y) v)
                nxt
                (reduced nxt))))
          0 ys))

(defn ->scorer [rows xmin xmax ymin ymax]
  (let [xy->v (fn [x y] (get-in rows [y x]))]
    (fn [x y]
      (let [v     (xy->v x y)
            [l r] (horizontal xmin xmax x)
            [u d] (vertical   ymin ymax y)
            sl (scorex xy->v v y l)
            sr (scorex xy->v v y r)
            su (scorey xy->v v x u)
            sd (scorey xy->v v x d)
            ]
        (* sl sr su sd)))))

(defn max-possible [xmin xmax ymin ymax x y]
  (let [l (- x xmin)
        r (- xmax x)
        u (- ymax y)
        d (- y ymin)]
    (* l r u d)))

(defn solve-8b []
  (let [big (->> (io/resource "day8input.txt")
                 slurp
                 parse)
        n   (count big)
        xmax (dec n)
        ymax xmax
        scr (->scorer big 0 xmax 0 ymax)]
    (->> (optimal-order big)
         (reduce (fn [[best xy :as acc] [x y]]
                   (let [mx (max-possible 0 xmax 0 ymax x y)]
                     (if (> best mx)
                       ;;best found is better than max possible
                       ;;going fwd, stop.
                       (reduced [best xy])
                       (let [v (scr x y)]
                         (if (= v max-possible) ;;can't do better.
                           (reduced [v [x y]])
                           (if (< best v)
                             [v [x y]]
                             acc))))))
                 [0 nil]))))

(defn brute []
  (let [big (->> (io/resource "day8input.txt")
                 slurp
                 parse)
        n   (count big)
        xmax (dec n)
        ymax xmax
        scr (->scorer big 0 xmax 0 ymax)]
    (->> (for [x (range n)
               y (range n)]
           [(scr x y) x y])
         (apply max-key first))))


