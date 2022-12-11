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

;;wait-time is how long to wait for the next instruction.
(defn step [{:keys [t state] :as env} op dt]
    (let [tnxt (unchecked-inc t)]
      (if (and (pos? t) (empty? new-pending))
        (reduced (assoc env :t tnxt :state new-state :pending {}))
        ;;push instruction onto the queue.
        (assoc env
               :t       tnxt
               :state   new-state
               :pending (if dt
                          (update new-pending dt
                                  (fn [coll] (conj (or coll []) mv)))
                          ;;execute.
                          new-pending)))))

(defn process [init xs]
  (reductions step init (concat xs (repeat nil))))

(def init {:t 0 :state {:x 1} :pending '()})


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

(defn signal-strength [{:keys [t state]}]
  (-> state :x (* t)))

(defn signal-cycles [mvs]
  (->> mvs
       (process init)
       (drop )
       (take 1)
       #_(map signal-strength)))
