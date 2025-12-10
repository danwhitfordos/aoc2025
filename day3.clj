(require '[utils]
         '[clojure.string :as str]
         '[clojure.math :as math])

(def test-input "987654321111111
811111111111119
234234234234278
818181911112111")

(defn parse-bank [bank]
  (map Integer/parseInt (str/split bank #"")))

(defn max-digit [s]
  (loop [[hd & rst] s index 0 max 0 max-index 0]
    (cond
      (nil? hd) [max max-index]
      (> hd max) (recur rst (inc index) hd index)
      :else (recur rst (inc index) max max-index))))

(assert (= (max-digit '(9 8 7 6 5)) [9 0]))

(defn highest-joltage [bank]
  (let [l (count bank)
        [max-first max-first-idx] (max-digit (take (dec l) bank))
        [max-two _max-two-idx] (max-digit (drop (inc max-first-idx) bank))]
    ;; (println max-first max-two)
    (+ (* max-first 10) max-two)))

(assert (= (highest-joltage 
            '(9 8 7 6 5 4 3 2 1 1 1 1 1 1 1)) 
           98))

(let [banks (map parse-bank (str/split-lines test-input))
      jolts (map highest-joltage banks)]
  (println jolts)
  (apply + jolts))


(let [banks (map parse-bank (str/split-lines (slurp "day3.txt")))
      jolts (map highest-joltage banks)]
;;   (println jolts)
  (apply + jolts))

(defn highest-joltage2 [bank]
  (loop [steps 12 total 0 bank bank] 
    (cond
      (= steps 0) (long total)
      :else (let 
             [l (count bank)
              available (take (- l (dec steps)) bank)
              [max max-idx] (max-digit available)
              digit (* max (math/pow 10 (dec steps)))]
            ;;   (println max digit available bank)
              (recur (dec steps) (+ total digit) (drop (inc max-idx) bank))))))

(highest-joltage2 '(9 8 7 6 5 4 3 2 1 1 1 1 1 1 1))
(highest-joltage2 '(2 3 4 2 3 4 2 3 4 2 3 4 2 7 8))

(let [banks (map parse-bank (str/split-lines test-input))
      jolts (map highest-joltage2 banks)]
  ;;   (println jolts)
  (apply + jolts))

(let [banks (map parse-bank (str/split-lines (slurp "day3.txt")))
      jolts (map highest-joltage2 banks)]
  ;;   (println jolts)
  (apply + jolts))