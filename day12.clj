(require '[utils]
         '[clojure.string :as str])

(defn parse-line [line]
  (let [bits (str/split line #"[:]? ")
        area (map Integer/parseInt (str/split (first bits) #"x"))
        nshapes (map Integer/parseInt (rest bits))] 
    {:area area :nshapes nshapes}))

(defn parse [input]
  (map parse-line (drop 30 (str/split-lines input))))

(defn fits? [line]
  (let [area (apply * (:area line))
        present-area (map #(* 9 %) (:nshapes line))
        total-present-area (apply + present-area)]
   (>= area total-present-area)))

(let [presents (parse (slurp "day12.txt"))]
  (count (filter fits? presents)))
