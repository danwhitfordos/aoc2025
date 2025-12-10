(require '[utils]
         '[clojure.string :as str]
         '[clojure.data.priority-map :as pm]
         '[clojure.math :as math]
         '[clojure.set :as set])

(def test-input "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")

(defn parse-point [line]
  (vec (map Integer/parseInt (str/split line #","))))

(defn parse [input]
  (let [points (map parse-point (str/split-lines input))]
    points))

(defn distance [p1 p2]
  (let [[x1 y1 z1] p1 [x2 y2 z2] p2]
    (math/sqrt (+
                (math/pow (- x2 x1) 2)
                (math/pow (- y2 y1) 2)
                (math/pow (- z2 z1) 2)))))

(distance [0 0 0] [2 2 2])

(defn all-distances [points]
  (let [pq (pm/priority-map)]
    (into pq (for [x points y points :when (not= x y)]
               [(sort [x y]) (distance x y)]))))

(defn find-circuit [p circuits]
  (loop [[c & rst] circuits]
    (cond
      (nil? c) (println "oh no")
      (contains? c p) c
      :else (recur rst))))

(defn join-boxes [distances circuits n]
  (loop [[[[p1 p2] _dist] & rst] distances
         circuits circuits
         n n]
    ;; (println "smol" p1 p2 _dist)
    (if (<= n 0) circuits
        (let [c1 (find-circuit p1 circuits)
              c2 (find-circuit p2 circuits)]
          (recur rst
                 (conj (->> circuits
                            (filter #(not= % c1))
                            (filter #(not= % c2)))
                       (set/union c1 c2))
                 (dec n))))))

(let [points (parse test-input)
      distances (all-distances points)
      circuits (map hash-set points)
      joined-circuits (join-boxes distances circuits 10)]
  (->> (map count joined-circuits)
       (sort)
       (take-last 3)
       (apply *)))

(let [points (parse (slurp "day8.txt"))
      distances (all-distances points)
      circuits (map hash-set points)
      joined-circuits (join-boxes distances circuits 1000)]
  (->> (map count joined-circuits)
       (sort)
       (take-last 3)
       (apply *)))

;; part 2

(defn join-until-complete [distances circuits]
  (loop [[[[p1 p2] _dist] & rst] distances
         circuits circuits]
    ;; (println "smol" p1 p2)
    (let [c1 (find-circuit p1 circuits)
          c2 (find-circuit p2 circuits)
          new-circ (conj (->> circuits
                              (filter #(not= % c1))
                              (filter #(not= % c2)))
                         (set/union c1 c2))]
      (if (= (count new-circ) 1) [p1 p2 (* (first p1) (first p2))]
          (recur rst new-circ)))))

(let [points (parse test-input)
      distances (all-distances points)
      circuits (map hash-set points)]
  (join-until-complete distances circuits))

;; 216,146,977 and 117,168,530

(let [points (parse (slurp "day8.txt"))
      distances (all-distances points)
      circuits (map hash-set points)]
  (join-until-complete distances circuits))
