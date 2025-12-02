
(require '[utils]
         '[clojure.string :as str])

(def test-input "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(str/split-lines test-input)

(defn parse-line [line]
  (let [eld (str/replace-first line "L" "-")
        ard (str/replace-first eld "R" "")]
    (Integer/parseInt ard)))

;; part 1
(let [lines (str/split-lines (slurp "day1.txt"))
      operations (map parse-line lines)]
  (loop [ops operations pos 50 count 0]
    (if
     (empty? ops) count
     (recur
      (rest ops)
      (mod (+ pos (first ops)) 100)
      (if (= pos 0) (inc count) count)))))

(defn passes [start op]
  (cond
    (>= op 0) (quot (+ start op) 100)
    (> (+ start op) 0) 0 ;; doesn't pass 0
    (= start 0) (quot (+ start op) -100)
    :else (inc (quot (+ start op) -100))))

(assert (= (passes 68 82) 1))
(assert (= (passes 60 55) 1))
(assert (= (passes 1 -99) 1))
(assert (= (passes 52 48) 1))
(assert (= (passes 50 1000) 10))
(assert (= (passes 50 -1000) 10))
(assert (= (passes 50 -50) 1))
(assert (= (passes 50 -150) 2))
(assert (= (passes 50 -250) 3))
(assert (= (passes 90 9) 0))
(assert (= (passes 99 1) 1))
(assert (= (passes 1 -1) 1))
(assert (= (passes 1 -2) 1))
(assert (= (passes 50 -50) 1))
(assert (= (passes 1 -10) 1))
(assert (= (passes 0 10) 0))
(assert (= (passes 0 -10) 0))
(assert (= (passes 0 -99) 0))
(assert (= (passes 0 -100) 1))
(assert (= (passes 82 -30) 0))

(defn inc-count [pos op count]
  (let [npasses (passes pos op)]
    (+ npasses count)))

;; part 2
(let [lines (str/split-lines test-input)
      operations (map parse-line lines)]
  (loop [ops operations pos 50 count 0]
    (println pos (first ops) count)
    (if
     (empty? ops) count
     (recur
      (rest ops)
      (mod (+ pos (first ops)) 100)
      (inc-count pos (first ops) count)))))

(let [lines (str/split-lines (slurp "day1.txt"))
      operations (map parse-line lines)]
  (loop [ops operations pos 50 count 0]
    ;; (println pos (first ops) count)
    (if
     (empty? ops) count
     (recur
      (rest ops)
      (mod (+ pos (first ops)) 100)
      (inc-count pos (first ops) count)))))

;; bad - 5396, 
;; right 6386
