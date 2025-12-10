(require '[utils]
         '[clojure.string :as str]
         '[clojure.math :as math])

(def test-input "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")

(defn transpose [m]
  (apply mapv vector m))

(defn filter-blank [coll]
  (filter #(not= "" %) coll))

(defn parse-line [line]
  (let [l (count line)]
    {:nn (map Integer/parseInt (take (dec l) line))
     :op (nth line (dec l))}))

(defn eval-line [line]
  (cond
    (= (:op line) "+") (apply + (:nn line))
    (= (:op line) "*") (apply * (:nn line))))

(defn solve [input]
  (let [lines (str/split-lines input)
        lines (map #(str/split % #" ") lines)
        lines (map filter-blank lines)
        lines (map parse-line (transpose lines))]
    (apply + (map eval-line lines))))

(solve test-input)
(solve (slurp "day6.txt"))

;; part 2

(defn new-calc [op nns]
  {:nn [nns] :op op})

(defn parse-nns [line]
  (Integer/parseInt (str/trim (apply str (pop line)))))

(defn parse-calcs [lines]
  (loop [[hd & rst] lines calcs [] this-calc {}]
    (cond
      (nil? hd) (conj calcs this-calc)
      (= (peek hd) \+) (recur
                        rst
                        calcs
                        (new-calc "+" (parse-nns hd)))
      (= (peek hd) \*) (recur
                        rst
                        calcs
                        (new-calc "*" (parse-nns hd)))
      (every? #(= \space %) hd) (recur
                                 rst
                                 (conj calcs this-calc)
                                 {})
      :else (recur
             rst
             calcs
             (conj this-calc {:nn (conj (:nn this-calc) (parse-nns hd))})))))

(defn solve2 [input]
  (let [lines (str/split-lines input)
        lines (transpose lines)
        lines (parse-calcs lines)]
    (apply + (map eval-line lines))))

(solve2 test-input) ;;3263827
(solve2 (slurp "day6.txt"))
