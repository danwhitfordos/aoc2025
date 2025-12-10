(require '[utils]
         '[clojure.string :as str]
         '[clojure.math :as math])

(def test-input ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

(defn parse-start [line]
  (str/index-of line "S"))

(defn parse-filter [line]
  (loop [idx 0 [hd & rst] line filters (hash-set)]
    (cond
      (nil? hd) filters
      (= hd \^) (recur (inc idx) rst (conj filters idx))
      :else (recur (inc idx) rst filters))))

(defn parse-filters [lines]
  (vec (map parse-filter lines)))

(defn parse [input]
  (let [lines (str/split-lines input)
        start (parse-start (first lines))
        filters (parse-filters (rest lines))]
    {:start start :filters filters}))

(parse test-input)

(defn apply-filter [beams filter]
  (loop [[beam & rst] beams
         res (hash-set)
         hits 0]
    (cond
      (nil? beam) [res hits]
      (contains? filter beam) (recur
                               rst
                               (conj (conj res (inc beam)) (dec beam))
                               (inc hits))
      :else (recur rst (conj res beam) hits))))

(apply-filter #{7} #{7})
(apply-filter #{7} #{0})
(apply-filter #{6 8} #{7})

(let [board (parse test-input)]
  (loop [beams (hash-set (:start board))
         [filter & rst] (:filters board)
         hits 0]
    (let [[next hits2] (apply-filter beams filter)]
      ;;   (println beams filter hits hits2)
      (cond
        (nil? filter) hits
        :else (recur next rst (+ hits hits2))))))

(let [board (parse (slurp "day7.txt"))]
  (loop [beams (hash-set (:start board))
         [filter & rst] (:filters board)
         hits 0]
    (let [[next hits2] (apply-filter beams filter)]
      ;;   (println beams filter hits hits2)
      (cond
        (nil? filter) hits
        :else (recur next rst (+ hits hits2))))))

;; part2

(defn tl-done? [tl]
  (empty? (:filters tl)))

(defn step-tl [tl]
  (assert (not (tl-done? tl)))
  (let [[f & rst] (:filters tl)
        beam (:start tl)]
    (cond
      (contains? f (:start tl)) [{:start (inc beam) :filters rst}
                                 {:start (dec beam) :filters rst}]
      :else [{:start beam :filters rst}])))

(let [board (parse test-input)]
  (loop [[tl & rst] [board]
         timelines-done 0]
    (cond
      (nil? tl) timelines-done
      (tl-done? tl) (recur rst (inc timelines-done))
      :else (recur (concat rst (step-tl tl)) timelines-done))))

(defn count-lifetimes [tl cache]
  (let [beam (:start tl)
        [f & rst] (:filters tl)]
    ;; (println (deref cache))
    ;; (println beam f)
    (cond
      (tl-done? tl) 1
      (contains? (deref cache) tl) ((deref cache) tl)
      (contains? f beam)
      ((swap! cache assoc tl (+
                             (count-lifetimes
                              {:start (dec beam)
                               :filters rst}
                              cache)
                             (count-lifetimes
                              {:start (inc beam)
                               :filters rst}
                              cache))) tl)
      :else ((swap! cache assoc tl
                   (count-lifetimes {:start beam :filters rst} cache)) tl))))

(let [tl (parse test-input)]
  (count-lifetimes tl (atom {})))

(let [tl (parse (slurp "day7.txt"))]
  (count-lifetimes tl (atom {})))
