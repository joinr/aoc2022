(ns aoc2022.day7
  (:require [clojure.java.io :as io]))

(def sample
"$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

;;simple rules for parsing terminal session:
;;$ cmd arg*

;;if cmd is ls, we know the following lines can be
;;dir | file.

;;can parse as take-while $..$
;;group successive non-command lines as output from last cmd.

(defn parse-command [cmd]
  (let [[_ cmd arg] (clojure.string/split cmd #" ")]
    (if arg
      {:cmd cmd :arg arg}
      {:cmd cmd})))

(defn parse [txt]
  (->> txt
       clojure.string/split-lines
       (partition-by (fn [ln] (case (nth ln 0) \$ :cmd :res)))
       (reduce (fn [acc xs]
                 (if (= (ffirst xs) \$)
                   (into acc (map parse-command xs))
                   (conj acc {:result xs}))) [])
       (partition 2 1)
       (reduce (fn [acc [l r]]
                 (cond (= (l :cmd) "ls")
                         (conj acc (assoc l :result (r :result)))
                       (not (l :result))
                       (conj acc l)
                       :else acc)) [])))

;;now we want a stateful process to record the path and
;;build a directory structure with annotate directory size.
;;will store our "file system" as {\\ {:size 0 :children {} }}
;;as we add children to the path, we propogate their size as well.

(def root-fs {"/" {:name "/" :size 0 :children {}}})

(defn add-child [fs path child]
  (if-let [k (first path)]
    (let [parent (fs k {:name k :size 0 :children {}})
          new-children (add-child (parent :children) (rest path) child)
          new-parent   (-> parent
                           (assoc :size (+ (parent :size) (child :size))
                                  :children new-children))]
      (assoc fs k new-parent))
      (assoc fs (child :name) child)))

#_
(-> root-fs
    (add-child ["/" "blah.txt"] {:name "blah.txt" :size 10})
    (add-child ["/" "bleeh.txt"] {:name "blee.txt" :size 3})
    (add-child ["/" "a" "b" "c" "another.txt"] {:name "another.txt." :size 5}))

(defn do-cd [{:keys [path fs] :as state} arg]
  (assoc state :path
         (case arg
           "/"  [arg]
           ".." (pop path)
           (conj path arg))))

(defn parse-results [xs]
  (->> xs
       (mapv (fn [x]
               (let [[l r] (clojure.string/split x #" ")]
                 (if (= l "dir")
                   {:name r :directory true}
                   {:name r :size (parse-long l) :file true }))))))

(defn build-fs [root cmds]
  (->> cmds
       (reduce (fn [{:keys [path fs] :as state} cmd]
                 (case (cmd :cmd)
                   "cd" (do-cd state (cmd :arg))
                   "ls"
                   (let [children (parse-results (cmd :result))
                         new-fs   (reduce (fn [acc chld]
                                            (add-child acc path chld))
                                          fs
                                          (filter :file children))]
                     (assoc state :fs new-fs)))) {:path ["/"] :fs root})
       :fs))

(defn directories
  ([path root]
   (if-let [children (root :children)]
     (apply concat
            (for [[k v] children
                  :when (not (v :file))]
              (let [p (conj path k)]
                (concat [(-> v (dissoc :children) (assoc :path p))]
                        (directories p v)))))))
  ([root]
   (concat [(-> (root "/") (dissoc :children) (assoc :path ["/"]))]
           (directories ["/"] (root "/")))))

(defn smaller-than [n xs]
  (->> xs (filter (fn [r] (<= (r :size) n)))))

(def small (->> sample parse (build-fs root-fs)))

(def big (->> (io/resource "day7input.txt") slurp parse (build-fs root-fs)))

(defn solve-7a []
    (->> (directories big)
         (smaller-than 100000)
         (map :size)
         (reduce +)))


(def +total+ 70000000)
(def +update+ 30000000)

(defn needed [fs]
  (let [current (-> fs (get "/") :size)
        available (- +total+ current)]
    (- +update+ available)))

(defn bigger-than [n root]
  (->> (directories root)
       (filter (fn [r] (>= (r :size) n)))))

(defn solve-7b []
  (->> (bigger-than (needed big) big)
       (sort-by :size)
       first
       :size))
