(ns day11 (:require
           [utils]
           [clojure.string :as str]
           [clojure.math.combinatorics :as combo]))

(def test-input "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")

(defn parse-line [line]
  (let [[key outputs] (str/split line #": ")
        outputs (str/split outputs #" ")]
    ;; (println key outputs)
    [key outputs]))

(defn parse [input]
  (into {} (map parse-line (str/split-lines input))))

(defn paths [rack key path]
  ;;   (println key path)
  (cond
    (= key "out") [(conj path key)]
    :else (loop [[out & rst] (get rack key) res []]
            ;; (println (paths rack out path))
            (cond
              (nil? out) res
              :else (recur
                     rst
                     (concat res (paths rack out (conj path key))))))))

(let [rack (parse test-input)
      paths-to-out (paths rack "you" [])]
  (println paths-to-out)
  (count paths-to-out))

(let [rack (parse (slurp "day11.txt"))
      paths-to-out (paths rack "you" [])]
  ;;   (println paths-to-out)
  (count paths-to-out))

;; part 2

(def test-input2 "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")

(defn paths2 [rack key stopword visited]
  ;; (println key visited)
  (assert (some? key))
  (cond
    (contains? (deref visited) key) (get (deref visited) key)
    (= key stopword) 1
    (= key "out") 0
    :else (let [x (map #(paths2 rack % stopword visited) (get rack key))
                y (reduce + x)]
            (swap! visited assoc key y)
            y)))

(let [rack (parse test-input2)]
  (max (* (paths2 rack "svr" "fft" (atom {}))
          (paths2 rack "fft" "dac" (atom {}))
          (paths2 rack "dac" "out" (atom {})))
       (* (paths2 rack "svr" "dac" (atom {}))
          (paths2 rack "dac" "fft" (atom {}))
          (paths2 rack "fft" "out" (atom {})))))

(let [rack (parse (slurp "day11.txt"))]
  (max (* (paths2 rack "svr" "fft" (atom {}))
          (paths2 rack "fft" "dac" (atom {}))
          (paths2 rack "dac" "out" (atom {})))
       (* (paths2 rack "svr" "dac" (atom {}))
          (paths2 rack "dac" "fft" (atom {}))
          (paths2 rack "fft" "out" (atom {})))))
