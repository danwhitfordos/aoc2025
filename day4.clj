(require '[utils]
         '[clojure.string :as str]
         '[clojure.math :as math])

(def test-input "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(defn parse-row [y row]
  (loop [x 0 [col & rst] row papers (hash-set)]
    (cond
      (nil? col) papers
      (= \@ col) (recur (inc x) rst (conj papers [x y]))
      :else (recur (inc x) rst papers))))

(parse-row 0 "..@@.@@@@.")

(defn solve [input]
  (let [rows (str/split-lines input)]
    (loop [y 0 [row & row-rest] rows papers (hash-set)]
      (cond
        (nil? row) papers
        :else (recur
               (inc y)
               row-rest
               (apply conj papers (parse-row y row)))))))

(defn neighbours [[x y]]
  (for [x1 [(dec x) x (inc x)] y1 [(dec y) y (inc y)]
        :when (not= [x1 y1] [x y])]
    [x1 y1]))

(defn paper-neighbours [rolls neighbours]
  (count
   (clojure.set/intersection rolls (set neighbours))))

(count
 (str/split-lines test-input))
(count (first (str/split-lines test-input)))

(let [rolls (solve test-input)
      nebs (map #(neighbours %) rolls)
      pap-nebs (map #(paper-neighbours rolls %) nebs)]
  (count (filter #(< % 4) pap-nebs)))

(let [rolls (solve (slurp "day4.txt"))
      nebs (map #(neighbours %) rolls)
      pap-nebs (map #(paper-neighbours rolls %) nebs)]
  (count (filter #(< % 4) pap-nebs)))

;; part 2

(defn can-remove? [roll rolls]
  (let [nebs (neighbours roll)
        pap-nebs (paper-neighbours rolls nebs)]
    (< pap-nebs 4)))

(defn remove-papers [rolls]
  (clojure.set/difference
   rolls
   (filter #(can-remove? % rolls) rolls)))

(remove-papers #{[2 2]})

(let [rolls (solve test-input)]
  (loop [rolls rolls
         removed 0]
    (let [next-gen (remove-papers rolls)]
      (cond
        (= rolls next-gen) removed
        :else (recur
               next-gen
               (+ removed (- (count rolls) (count next-gen))))))))

(let [rolls (solve (slurp "day4.txt"))]
  (loop [rolls rolls
         removed 0]
    (let [next-gen (remove-papers rolls)]
      (cond
        (= rolls next-gen) removed
        :else (recur
               next-gen
               (+ removed (- (count rolls) (count next-gen))))))))
