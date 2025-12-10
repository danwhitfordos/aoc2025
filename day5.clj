(require '[utils]
         '[clojure.string :as str]
         '[clojure.math :as math])

(def test-input "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

(defn parse-range [range]
  (let [[start stop] (str/split range #"-")]
    {:min (Long/parseLong start)
     :max (Long/parseLong stop)}))

(defn parse-ranges [ranges]
  (map parse-range ranges))

(defn solve [input]
  (let [lines (str/split-lines input)
        [ranges items] (split-with #(not= "" %) lines)
        ranges (parse-ranges ranges)
        items (filter #(not= % "") items)
        items (map Long/parseLong items)]
    ;; (println ranges)
    ;; (println items)
    [ranges items]))

(defn in-range? [range item]
  (and (>= item (:min range))
       (<= item (:max range))))

(defn fresh? [ranges item]
  (some #(in-range? % item) ranges))

(fresh? [{:min 3 :max 5}] 4)

(let [[ranges items] (solve test-input)]
  (count (filter #(fresh? ranges %) items)))

(let [[ranges items] (solve (slurp "day5.txt"))]
  (count (filter #(fresh? ranges %) items)))

;; part 2

(defn overlap? [a b]
  (>= (:max a) (:min b)))

(overlap? {:min 5 :max 10} {:min 8 :max 50})
(overlap? {:min 5 :max 10} {:min 11 :max 50})
(overlap? {:min 5 :max 10} {:min 10 :max 50})

(defn merge [a b]
  {:min (min (:min a) (:min b))
   :max (max (:max a) (:max b))})

(defn merge-ranges [ranges]
  (loop [[hd td & rst] ranges
         merged []]
    (cond
      (nil? hd) merged
      (nil? td) (recur rst (conj merged hd))
      (overlap? hd td) (recur 
                        (cons (merge hd td) rst) 
                        merged)
      :else (recur
             (cons td rst)
             (conj merged hd)))))
(defn sum-ranges [ranges]
  (apply + (map #(- (inc (:max %)) (:min %)) ranges)))

(let [[ranges _] (solve test-input)
      ranges (sort-by :min ranges)]
  (sum-ranges (merge-ranges ranges)))

(let [[ranges _] (solve (slurp "day5.txt"))
      ranges (sort-by :min ranges)]
  (sum-ranges (merge-ranges ranges)))
