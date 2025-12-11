(require '[utils]
         '[clojure.string :as str])

(def test-input "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

(defn all-pairs [coll]
  (when-let [s (next coll)]
    (lazy-cat (for [y s] [(first coll) y])
              (all-pairs s))))

(defn parse-line [line]
  (map Integer/parseInt (str/split line #",")))

(defn parse [input]
  (->> (str/split-lines input)
       (map parse-line)))

(defn area [p1 p2]
  (let [[x1 y1] p1 [x2 y2] p2]
    (* (inc (abs (- x2 x1)))
       (inc (abs (- y2 y1))))))

(area [2 5] [11 1])

(let [tiles (parse test-input)
      pairs (all-pairs tiles)]
  (transduce
   (map #(apply area %))
   max
   0
   pairs))

(let [tiles (parse (slurp "day9.txt"))
      pairs (all-pairs tiles)]
  (transduce
   (map #(apply area %))
   max
   0
   pairs))

;; part 2

(defn horizontal? [_x1 y1 _x2 y2]
  (= y1 y2))

(defn horizontal-overlap? [x1 y1 x2 y2 x3 y3 x4 y4]
  (assert (= y3 y4))
  (and
   (<= y3 (max y1 y2))
   (>= y3 (min y1 y2))
   (or
    (and (< (min x3 x4) (min x1 x2)) (> (max x3 x4) (max x1 x2))) ;; strikethrough
    (and (< (min x3 x4) (min x1 x2)) (> (max x3 x4) (min x1 x2))) ;; left
    (and (< (min x3 x4) (max x1 x2)) (> (max x3 x4) (max x1 x2)))))) ;; right

(horizontal-overlap? 0 0 5 5 0 0 5 0)
(horizontal-overlap? 0 0 5 5 0 0 10 0)
(horizontal-overlap? 5 5 10 10 0 5 15 5)
(horizontal-overlap? 5 5 10 10 0 10 8 10)
(horizontal-overlap? 5 5 10 10 9 10 15 10)

(defn vertical-overlap? [x1 y1 x2 y2 x3 y3 x4 y4]
  (assert (= x3 x4))
  (and
   (<= x3 (max x1 x2))
   (>= x3 (min x1 x2))
   (or
    (and (< (min y3 y4) (min y1 y2)) (> (max y3 y4) (max y1 y2))) ;; strikethrough
    (and (< (min y3 y4) (min y1 y2)) (> (max y3 y4) (min y1 y2))) ;; left
    (and (< (min y3 y4) (max y1 y2)) (> (max y3 y4) (max y1 y2)))))) ;; right

(defn line-intersects-rect? [x1 y1 x2 y2 [[x3 y3] [x4 y4]]]
  ;; (println x1 y1 x2 y2 x3 y3 x4 y4)
  (cond
    (horizontal? x3 y3 x4 y4) (horizontal-overlap? x1 y1 x2 y2 x3 y3 x4 y4)
    :else (vertical-overlap? x1 y1 x2 y2 x3 y3 x4 y4)))

(line-intersects-rect? 5 5 10 10 [[0 6] [10 6]])
(line-intersects-rect? 5 5 10 10 [[5 5] [15 5]])
(line-intersects-rect? 5 5 10 10 [[5 5] [10 5]])

(defn lines-from-poly-points [poly-points]
  (loop [[hd td & rst] poly-points lines []]
    (cond
      (nil? hd) lines
      (nil? td) (recur rst (conj lines [hd (first poly-points)]))
      :else (recur (cons td rst) (conj lines [hd td])))))

(lines-from-poly-points '((0 0) (5 0) (5 5) (0 5)))

(defn rect-inside-poly? [x1 y1 x2 y2 poly-points]
  (let [poly-lines (lines-from-poly-points poly-points)]
    (not-any? #(line-intersects-rect? x1 y1 x2 y2 %) poly-lines)))

(rect-inside-poly? 9 5 2 3 '((7 1) (11 1) (11 7) (9 7) (9 5) (2 5) (2 3) (7 3)))
(rect-inside-poly? 2 5 11 1 '((7 1) (11 1) (11 7) (9 7) (9 5) (2 5) (2 3) (7 3)))
(rect-inside-poly? 9 7 2 3 '((7 1) (11 1) (11 7) (9 7) (9 5) (2 5) (2 3) (7 3)))

(defn pair-in-polygon? [[p1 p2] poly]
  (let [[x1 y1] p1 [x2 y2] p2]
    (rect-inside-poly? x1 y1 x2 y2 poly)))

(let [red-tiles (parse test-input)
      pairs (all-pairs red-tiles)
      filt (filter #(pair-in-polygon? % red-tiles) pairs)]
  ;; (println filt)
  (map #(vector % (apply area %)) filt))
