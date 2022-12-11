(ns aoc2022.day9
  (:require [clojure.java.io :as io]
            [clojure.math :as m]))



(def sample
"R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defn parse [txt]
  (->> txt
       clojure.string/split-lines
       (map (fn [x] (let [[c n] (clojure.string/split x #" ")]
                      [(keyword c) (parse-long n)])))))

;;observation: any time the head an tail are "touching", we
;;can move the head.  That means there is a neighborhood
;;of moves within distance 1 where the tail will move.
;;Also, most of the time, tail moves to where head was.

(defn vdiff [[x1 y1] [x2 y2]]
  [(- x2 x1) (- y2 y1)])

(defn dist [l r]
  (->> (vdiff l r)
       (transduce (map abs) max 0)))

(def diag #{:UL :UR :DL :DR})

(defn step
  ([dir [hx hy] n]
   (case dir
     :R  [(+ hx n)  hy]
     :L  [(- hx n)  hy]
     :U  [hx  (+ hy n)]
     :D  [hx  (- hy n)]
     :UR [(+ hx n) (+ hy n)]  ;;we now know diagonals.
     :DR [(+ hx n) (- hy n)]
     :UL [(- hx n)  (+ hy n)]
     :DL [(- hx n)  (- hy n)]))
  ([dir v] (step dir v 1)))

(defn move-tail [[hx hy :as h] [tx ty :as t]]
  ;;move the tail!
  (cond (= hx tx) ;;same col, move vertical
          (if (< hy ty)
            (step :D t)
            (step :U t))
        (= hy ty) ;;same row, move horizontal
          (if (< hx tx)
            (step :L t)
            (step :R t))
        ;;diagonals
        (< hx tx)
        (if (< hy ty)
          (step :DL t)
          (step :UL t))
        (> hx tx)
        (if (< hy ty)
          (step :DR t)
          (step :UR t))))

(defn tail-moved [[x1 y1] [x2 y2]]
  (cond (= x1 x2) (if (< y1 y2) :U :D)
        (= y1 y2) (if (< x1 x2) :R :L)
        (< x1 x2) (if (< y1 y2) :UR :DR)
        (< x2 x1) (if (< y1 y2) :UL :DL)))

;;helper.
(defn render [w h {:keys [head tail d]}]
  (let [grid (-> (vec (for [rows (range h)]
                        (vec (repeat w "."))))
                 (assoc-in (reverse tail) "T")
                 (assoc-in (reverse head) "H"))]
    (doseq [r (reverse grid)]
      (println r))))

(defn move
  ([{:keys [head tail d] :as state} dir]
   (let [[hx hy] head
        [nx ny :as hnew]  (step dir head)
         dnew (dist hnew tail)
         new-tail (when (> dnew 1)
                    (move-tail  hnew tail))
         dcurr (or (when new-tail (dist hnew new-tail))
                   dnew)]
     (as-> state it
       (assoc it :head hnew :d dcurr)
       (if new-tail
         (assoc it :tail new-tail :tail-moved (tail-moved tail new-tail))
         (dissoc it :tail-moved)))))
  ([state dir n]
   (if (zero? n)
     state
     (recur (move state dir) dir (unchecked-dec n)))))


(def init {:head [0 0] :tail [0 0] :d 0})
(defn walk [init xs]
  (let [current  (atom init)
        do-move! (fn [[dir steps]]
                   (swap! current move dir steps))]
    (->> (for [[dir steps] xs]
           (repeat steps [dir 1]))
         (apply concat)
         (map do-move!))))

(defn solve-9a []
  (->> (io/resource "day9input.txt")
       slurp
       parse
       (walk init)
       (map :tail)
       distinct
       count))

;;given an init, we can generate n children.
;;we already get 1.
(defn knots [n]
  (vec (for [i (range n)]
         (assoc init :idx i))))

(defn pull [ks dir steps]
  (->> ks
       (reduce (fn [[acc dir steps] {:keys [idx] :as state}]
                 (let [res (move state dir steps)]
                   (if-let [new-dir (res :tail-moved)]
                     (let [new-steps (dist (res :tail) (state :tail))]
                           [(assoc acc idx res) new-dir new-steps])
                     ;;else done.
                     (reduced [(assoc acc idx res) nil nil]))))
               [ks dir steps])
       first))

(defn pulls [ks mvs]
  (let [acc (atom ks)]
    (->> mvs
         (mapcat (fn [[dir steps]]
                   (repeat steps [dir 1])))
         (map (fn [[dir step]]
                (swap! acc pull dir step))))))

(defn coords [ks]
  (-> (->> ks (mapv :head))
      (conj (-> ks peek :tail))))

(defn render [w h ks]
  (let [grid (vec (for [rows (range h)]
                    (vec (repeat w "."))))
        grid (->> ks
                  coords
                  (map-indexed vector)
                  (reverse)
                  (reduce (fn [acc [n [x y]]]
                            (assoc-in acc [y x] (str n)))
                          grid))]
    (doseq [r (reverse grid)]
      (println r))))


(def sample2
"R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(defn solve-9b []
  (->> (io/resource "day9input.txt")
       slurp
       parse
       (pulls (knots 9))
       (map #(-> % (nth 8) :tail))
       distinct
       count))
