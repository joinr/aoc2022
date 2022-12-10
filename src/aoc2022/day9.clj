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
                      {:dir (keyword c) :steps (parse-long n)})))))

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

(defn step [dir [hx hy] n]
  (case dir
    :R  [(+ hx n)  hy]
    :L  [(- hx n)  hy]
    :U  [hx  (+ hy n)]
    :D  [hx  (- hy n)]
    :UR [(+ hx n) (+ hy n)]  ;;we now know diagonals.
    :DR [(+ hx n) (- hy n)]
    :UL [(- hx n)  (+ hy n)]
    :DL [(- hx n)  (- hy n)]))

(defn move-tail [dir [hx hy :as h] hprev [tx ty :as t]]
  ;;move the tail!
  (cond (= hx tx) ;;same col, move up
        (case dir :U
              [tx (inc ty)]
              [tx (dec ty)])
        (= hy ty) ;;same row, move side
        (case dir :L
              [(dec tx) ty]
              [(inc tx) ty])
        ;;diagonal
        (not (diag dir)) hprev
        :else (step dir t 1)))

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

(defn move [{:keys [head tail d] :as state} dir steps]
  (let [[hx hy] head
        n 1
        [nx ny :as hnew]  (step dir head n)
        dnew (dist hnew tail)
        new-tail (when (> dnew 1)
                   (move-tail dir hnew head tail))
        dcurr (or (when new-tail (dist hnew new-tail))
                  dnew)
        res (as-> state it
              (assoc it :head hnew :d dcurr)
              (if new-tail
                (assoc it :tail new-tail :tail-moved (tail-moved tail new-tail))
                (dissoc it :tail-moved)))]
    (if (= steps 1)
      res
      (recur res dir (unchecked-dec steps)))))


(def init {:head [0 0] :tail [0 0] :d 0})
(defn walk [init xs]
  (let [current  (atom init)
        do-move! (fn [[dir steps]]
                   (swap! current move dir steps))]
    (->> (for [{:keys [dir steps]} xs]
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

;;we can naively simulate 9 dependent walks.
;;Instead of one tail, we have a vector of tails.
;;The big change is that tails can move diagonally to catch up to predecessors.

;;So if the tail moved diagonally, we may like to record which direction
;;the tail moved as part of the state.

;;This allows us to pass information down to other tails,
;;and gives us a simpler movement enforcement.

(defn follow [parent-walk]
  (->> parent-walk
       (keep :tail-moved)
       (map (fn [dir] {:dir dir :steps 1}))
       (walk init)))

;;more work, no filtering.
(defn follow-debug [parent-walk]
  (->> parent-walk
       (map (fn [dir] {:dir dir :steps 1}))
       (walk init)))

(defn follow-n [n parent-walk]
  (loop [idx 0
         acc parent-walk]
    (if (= idx n)
      acc
      (recur (unchecked-inc idx) (follow acc)))))

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


(defn coords [ks]
  (->> ks
       (mapv :head)))

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
  (->> #_#_(io/resource "day9input.txt")
       slurp
       sample2
       parse
       (knots 9)
       (map :tail)
       #_#_distinct
       count))
