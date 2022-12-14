(ns aoc2022.day10
  (:require [clojure.java.io :as io]))

(def sample
"noop
addx 3
addx -5")

(defn parse [txt]
  (->> txt
       clojure.string/split-lines
       (map #(clojure.string/split % #" "))
       (map (fn [v]
              (into [(keyword (first v))] (map parse-long) (rest v))))))

(def cycles {:noop 1
             :addx 2})

;;we maintain a map of pending ops.
;;ops affect the state.
;;accumulate state over cycle time.

(defn do-op [state op-args]
  (case (op-args 0)
    :noop state
    :addx (update state :x + (op-args 1))))

;;check if any pending have completed.
;; ;;if pending completed, apply them to the state prior to new ops.
;;add new ops to the pending.

;;instructions correspond to adding time and modifying state.
(defn step [{:keys [t state] :as env} ops]
  (when-let [op (first ops)]
    (let [dt        (-> op first cycles)
          new-env   (-> env
                        (assoc  :t (+ t dt)
                                :state (do-op state op)))]
      (lazy-seq
       (cons new-env (step new-env (rest ops)))))))

(defn discrete-values
  ([init ops]
   (->> ops
        (step init)
        (cons init)
        (map (fn [s]
               (update s :t inc)))))
  ([ops] (discrete-values init ops)))

(def init {:t 0 :state {:x 1}})


(def big-sample
  "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

;;we can traverse
(defn sample-at
  ([t prev history]
   (if (= (prev :t) t)
     [prev history]
     (if-let [nxt (first history)]
      (if (> (nxt :t) t)
        [(assoc prev :t t) history]
        (recur t nxt (rest history)))
      [(assoc prev :t t) nil])))
  ([t history] (sample-at  t (first history) (rest history))))

(defn signal-samples [n xs]
  (->> (iterate (fn [[t x history]]
                  (let [[x nxt] (sample-at t history)]
                    [(+ t 40) x nxt]))
                [20 nil xs])
       (drop 1)
       (take n)
       (map second)))

(defn signal-strength [m]
  (-> m :state :x (* (m :t))))

(defn strengths [n xs]
  (->> xs
       (signal-samples n)
       (map signal-strength)))

(defn total-strength [n xs]
  (->> xs (strengths n) (reduce +)))

(defn solve-10a []
  (->> (io/resource "day10input.txt")
       slurp
       parse
       discrete-values
       (total-strength 6)))


;;part 2
;;we know the x coordinate based on state.
;;for each pixel at t, we have x (or can get it.)

;;x determines sprite's middle position, sprite
;;is 3 wide.

;;For any given t, is the pixel # or . ?
;;  for a given t, {:t n {:state {:x k}}}
;;  is (mod t 39) in the interval [k-1 k+1] ?

;;40 points per line.
;;init at x = 1.
;;0..39 coordinate space.
;;so t - 1.
(defn visible? [t x]
  (let [pos (mod (dec t) 40)] ;;position in screen space.
    (and (>= pos (- x 1))
         (<= pos (+ x 1)))))

(defn expand-signal [xs]
  (let [final (atom nil)]
    (-> (->> xs
             (partition 2 1)
             (mapcat (fn [[l r]]
                       (reset! final r)
                       (let [rt (r :t)
                             lt (l :t)
                             dt (- rt lt)]
                         (map (fn [t] (assoc l :t t)) (range lt (+ dt lt))))))
             vec)
        (conj @final))))

(defn get-pixels [xs]
  (->> xs
       expand-signal
       (map (fn [r]
              (if (visible? (r :t) (-> r :state :x))
                "#" ".")))
       (partition 40)
       (map vec)))


(defn solve-9b []
  (doseq [row (->> (io/resource "day10input.txt")
                   slurp
                   parse
                   discrete-values
                   expand-signal
                   get-pixels)]
    (println (apply str row))))
