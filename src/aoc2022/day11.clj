(ns aoc2022.day11
  (:require [clojure.java.io :as io]))

(def sample
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(defn as-op [op arg]
  (let [arg (clojure.edn/read-string arg)]
    (case arg
      'old (case op
             "*" (fn sqr  [n] (try (* n n )
                                   (catch Exception e (throw (ex-info "can't square!" {:in n})))))
             "+" (fn dbl  [n] (+ n n)))
      (case op
        "*" (fn mult [n] (* n arg))
        "+" (fn add [n] (+ n arg))))))

(defn parse-monkey [xs]
  (->> xs
       (map clojure.string/trim)
       (apply (fn [m items op test t f]
                {:m (->> m (re-find #"[0-9]+")  parse-long)
                 :seen 0
                 :items (-> items
                            (clojure.string/replace "Starting items: " "[")
                            (str "]")
                            (clojure.edn/read-string))
               :op  (let [phrase (re-find #"new = old . ([0-9]+|old)" op)
                          [_ _ _ op arg] (clojure.string/split (first phrase) #" " )]
                      (as-op op arg))
               :test (->> test
                          (re-find #"[0-9]+")
                          clojure.edn/read-string)
               :t (->> t (re-find #"[0-9]+")  parse-long)
               :f (->> f (re-find #"[0-9]+")  parse-long)}))))

(defn parse [txt]
  (->> txt
       clojure.string/split-lines
       (partition-by clojure.string/blank?)
       (filter #(not= (first %) ""))
       (mapv parse-monkey)
       (into [])))


(defn do-monkey [{:keys [items op test t f]}]
  (map (fn [n]
         (let [wl (quot (op n) 3)]
           [wl (if (zero? (rem wl test))
                   t
                   f)])) items))

(defn do-round
  ([f ms]
   (reduce-kv (fn [acc idx _]
                (let [monkey (acc idx)
                      new-items (f monkey)
                      new-monkey (->  monkey
                                      (update :seen + (count (monkey :items)))
                                      (assoc  :items []))]
                  (reduce (fn finalize [acc [item tgt]]
                            (update-in acc [tgt :items] conj item))
                          (assoc acc idx new-monkey) new-items)))
              ms ms))
  ([ms] (do-round do-monkey ms)))

(defn rounds
  ([f ms] (iterate #(do-round f %) ms))
  ([ms]  (rounds do-monkey ms)))

(defn monkey-business
  ([n ms] (monkey-business n do-monkey ms))
  ([n f ms]
   (->> ms
        (rounds f)
        (take (inc n))
        last
        (map :seen)
        (sort-by -)
        (take 2)
        (reduce *))))

(defn solve-11a []
  (->> (io/resource "day11input.txt")
       slurp
       parse
       (monkey-business 20)))

;;ugh, modular arithmetic.  not happy.
(defn mod-monkey [sm {:keys [items op test t f]}]
  (map (fn [n]
         (let [n  (mod n sm)
               wl (mod (op n) sm)]
           [wl (if (zero? (mod wl test))
                 t
                 f)])) items))

(defn solve-11b []
  (let [monks (->> (io/resource "day11input.txt")
                   slurp
                   parse)
        sm   (->> monks (map :test) (reduce *))
        f  (fn [m] (mod-monkey sm m))]
    (monkey-business 10000 f monks)))



