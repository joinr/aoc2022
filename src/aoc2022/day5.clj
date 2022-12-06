(ns aoc2022.day5
  (:require [clojure.java.io :as io]))


(def sample
"    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn parse-crates [xs]
  (let [ys (reverse xs)
        n  (count (clojure.string/split (first ys) #" "))
        offset 4 ;;] [
        idxs   (range 1 (inc (* n offset)) offset)
        row->crates (fn [row]
                      (mapv (fn [idx]
                              (nth row idx)) idxs))]
    (->> ys
         (map row->crates)
         (reduce (fn [acc xs]
                   (reduce (fn [acc [idx x]]
                             (if (not= x \space)
                               (update acc idx conj x)
                               acc))
                           acc
                           (map-indexed vector xs)))
                 (vec (repeat n '()))))))

(defn parse-move [x]
  (let [{:syms [move from to]} (clojure.edn/read-string (str "{" x "}"))]
    {:move move :from (dec from) :to (dec to)}))

(defn parse [txt]
  (let [total   (clojure.string/split-lines txt)
        [crates moves] [(take-while (fn [x]
                                      (not= (second x) \1)) total)
                        (drop-while (fn [x]
                                      (not= (first x) \m)) total)]]
    {:crates (parse-crates crates)
     :moves (mapv parse-move moves)}))

(defn move [{:keys [crates moves] :as state}]
  (let [{:keys [from to move]} (first moves)
        donor  (crates from)
        target (crates to)
        new-donor  (drop move donor)
        new-target (into target (take move donor))]
    {:crates (assoc crates from new-donor to new-target)
     :moves (rest moves)}))

(defn moves [state]
  (if (seq (state :moves))
    (recur (move state))
    state))

(defn msg [state]
  (->> state :crates (map peek) (apply str)))

(defn solve-5a []
  (->> (io/resource "day5input.txt")
       slurp
       parse
       moves
       msg))

;;change to use vectors.
(defn vparse [txt]
  (-> txt
      parse
      (update :crates (fn [xs] (mapv #(into [] (reverse %)) xs)))))

;;could use concatenative vecs (rrb trees),
;;to get efficient concat, but avoid astronaut engineering...

(defn vmove [{:keys [crates moves] :as state}]
  (let [{:keys [from to move]} (first moves)
        donor  (crates from)
        target (crates to)
        donated (if (= move 1)
                  (peek donor)
                  (subvec donor (- (count donor) move)))

        new-donor  (if (= move 1)
                     (pop donor)
                     (subvec donor 0 (- (count donor) move)))

        new-target (into target (if (vector? donated) donated [donated]))]
    {:crates (assoc crates from new-donor to new-target)
     :moves (rest moves)}))

(defn vmoves [state]
  (if (seq (state :moves))
    (recur (vmove state))
    state))

(defn solve-5b []
  (->> (io/resource "day5input.txt")
       slurp
       vparse
       vmoves
       msg))
