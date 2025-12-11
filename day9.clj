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

(defn point-inside? [[[x1 y1] [x2 y2]] [x3 y3]]
  (let [minx (min x1 x2) miny (min y1 y2)
        maxx (max x1 x2) maxy (max y1 y2)]
    ;; (println minx maxx miny maxy x2 y3)
    (and
     (not= [x1 y1] [x3 y3])
     (not= [x2 y2] [x3 y3])
     (> x3 minx)
     (< x3 maxx)
     (> y3 miny)
     (< y3 maxy))))

(point-inside? '((11 7) (2 3)) '(9 5))
(point-inside? '((9 7) (2 3)) '(9 5))
(point-inside? '((9 5) (2 3)) '(9 5))

(defn points-to-lines [points]
  (loop [[hd td & rst] points lines []]
    (cond
      (nil? hd) lines
      (nil? td) (recur rst (conj lines [hd (first points)]))
      :else (recur (cons td rst) (conj lines [hd td])))))

(points-to-lines '((0 0) (5 0) (5 5) (0 5)))
(points-to-lines (parse test-input))

(defn horizontal? [[p1 p2]]
  (let [[x1 y1] p1 [x2 y2] p2]
    (assert some? y1)
    (assert some? y2)
    (= y1 y2)))

(horizontal? '((0 5) (10 5)))
(horizontal? '((5 0) (5 10)))

(defn horizontal-overlap? [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (let [minx (min x1 x2) miny (min y1 y2)
        maxx (max x1 x2) maxy (max y1 y2)
        minxp (min x3 x4) maxxp (max x3 x4)]
    (assert (= y3 y4))
    (and
     (> y3 miny)
     (< y3 maxy)
     (or
      (and (>= minxp minx) (<= minxp maxx))
      (and (<= maxxp maxx) (>= maxxp minx))
      (and (<= minxp minx) (>= maxxp maxx))))))

(def rect '((5 5) (10 10)))
(assert (horizontal-overlap? rect '((6 6) (8 6))))
(assert (horizontal-overlap? rect '((0 6) (20 6))))
(assert (horizontal-overlap? rect '((5 6) (10 6))))
(assert (horizontal-overlap? rect '((4 6) (6 6))))
(assert (horizontal-overlap? rect '((9 6) (11 6))))
(assert (horizontal-overlap? rect '((5 6) (15 6))))
(assert (horizontal-overlap? rect '((9 6) (15 6))))
(assert (not (horizontal-overlap? rect '((5 5) (10 5)))))
(assert (not (horizontal-overlap? rect '((10 5) (15 5)))))
(assert (not (horizontal-overlap? rect '((0 5) (20 5)))))
(assert (not (horizontal-overlap? rect '((0 5) (5 5)))))
(assert (not (horizontal-overlap? rect '((10 5) (15 5)))))

(defn vertical-overlap? [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (let [minx (min x1 x2) miny (min y1 y2)
        maxx (max x1 x2) maxy (max y1 y2)
        minyp (min y3 y4) maxyp (max y3 y4)]
    (assert (= x3 x4))
    (and
     (> x3 minx)
     (< x3 maxx)
     (or
      (and (>= minyp miny) (<= minyp maxy))
      (and (<= maxyp maxy) (>= maxyp miny))
      (and (<= minyp miny) (>= maxyp maxy))))))

(assert (vertical-overlap? rect '((6 6) (6 8))))
(assert (vertical-overlap? rect '((6 0) (6 20))))
(assert (vertical-overlap? rect '((6 6) (6 8))))
(assert (vertical-overlap? rect '((6 6) (6 8))))
(assert (vertical-overlap? rect '((6 5) (6 10))))
(assert (vertical-overlap? rect '((6 5) (6 6))))
(assert (not (vertical-overlap? rect '((5 0) (5 20)))))

(defn intersects? [rect line]
  (or
   (point-inside? rect (first line))
   (point-inside? rect (second line))
   (and (horizontal? line) (horizontal-overlap? rect line))
   (and (not (horizontal? line)) (vertical-overlap? rect line))))

(assert (intersects? rect '((6 6) (6 10))))
(assert (intersects? rect '((6 10) (6 6))))
(assert (intersects? '((9 7) (9 5)) '((8 6) (10 6))))

(defn has-intersects? [rect lines]
  (some #(intersects? rect %) lines))

(has-intersects? '((9 7) (2 3)) '(((7 1) (11 1)) ((11 1) (11 7)) ((11 7) (9 7)) ((9 7) (9 5)) ((9 5) (2 5)) ((2 5) (2 3)) ((2 3) (7 3)) ((7 3) (7 1))))
(has-intersects? '((11 1) (2 3)) '(((7 1) (11 1)) ((11 1) (11 7)) ((11 7) (9 7)) ((9 7) (9 5)) ((9 5) (2 5)) ((2 5) (2 3)) ((2 3) (7 3)) ((7 3) (7 1))))
(has-intersects? '((9 7) (9 5)) '(((7 1) (11 1)) ((11 1) (11 7)) ((11 7) (9 7)) ((9 7) (9 5)) ((9 5) (2 5)) ((2 5) (2 3)) ((2 3) (7 3)) ((7 3) (7 1))))
(has-intersects? '((5 5) (10 10)) '(((5 6) (5 9))))
(has-intersects? '((5 5) (10 10)) '(((5 0) (5 20))))

(let [red-tiles (parse test-input)
      pairs (all-pairs red-tiles)
      green-lines (points-to-lines red-tiles)
      filt (filter #(not (has-intersects? % green-lines)) pairs)]
  (reduce max (map #(apply area %) filt)))

(let [red-tiles (parse test-input)
      pairs (all-pairs red-tiles)
      green-lines (points-to-lines red-tiles)]
  (assert (= (transduce
              (comp
               (filter #(not (has-intersects? % green-lines)))
               (map #(apply area %)))
              max
              0
              pairs)
             24)))

(let [red-tiles (parse (slurp "day9.txt"))
      pairs (all-pairs red-tiles)
      green-lines (points-to-lines red-tiles)]
  (transduce
   (comp
    (filter #(not (has-intersects? % green-lines)))
    (map #(apply area %)))
   max
   0
   pairs))

;; 4511989482 too high
;; 1467575648 wrong
;; ..............
;; .......#XXX#..
;; .......X...X..
;; ..#XXXX#...X..
;; ..X........X..
;; ..#XXXXXX#.X..
;; .........X.X..
;; .........#X#..
;; ..............

