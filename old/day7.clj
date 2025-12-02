(require '[utils]
         '[clojure.string :as str])

(def toy-in "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i")

(defn parse [lines]
  (loop [lines lines
         items (hash-map)]
    (let [head (first lines)]
      ;;   (println head)
      (cond
        (empty? lines) items
        (= (count head) 3)
        (recur
         (rest lines)
         (conj
          {(nth head 2)
           (first head)}
          items))
        (= (first head) "NOT")
        (recur
         (rest lines)
         (conj
          {(nth head 3)
           (take 2 head)}
          items))
        (= (count head) 5)
        (recur
         (rest lines)
         (conj
          {(nth head 4)
           (take 3 head)}
          items))
        :else "ERR SHOULDNT GET HERE"))))

(defn register-eval [key register res]
  (swap! register conj {key res})
  res)

(defn eval-circuit
  ([key circuit] (eval-circuit key circuit (atom (hash-map))))
  ([key circuit register]
  ;;  (println key circuit register)
   (if (contains? (deref register) key)
     ((deref register) key)
     (register-eval key register
                    (let [val (circuit key)]
                      (cond
                        (nil? val) (Integer/parseInt key)
                        (and
                         (seq? val)
                         (= (first val) "NOT")) (bit-not (eval-circuit (nth val 1) circuit register))
                        (and
                         (seq? val)
                         (= (nth val 1) "AND")) (bit-and 
                                                 (eval-circuit (nth val 0) circuit register) 
                                                 (eval-circuit (nth val 2) circuit register))
                        (and
                         (seq? val)
                         (= (nth val 1) "OR")) (bit-or 
                                                (eval-circuit (nth val 0) circuit register) 
                                                (eval-circuit (nth val 2) circuit register))
                        (and
                         (seq? val)
                         (= (nth val 1) "LSHIFT")) (bit-shift-left 
                                                    (eval-circuit (nth val 0) circuit register) 
                                                    (eval-circuit (nth val 2) circuit register))
                        (and
                         (seq? val)
                         (= (nth val 1) "RSHIFT")) (bit-shift-right 
                                                    (eval-circuit (nth val 0) circuit register) 
                                                    (eval-circuit (nth val 2) circuit register))
                        (re-matches #"[0-9]+" val) (Integer/parseInt val)
                        :else (eval-circuit val circuit register)))))))

(let [circuit (->> (str/split-lines toy-in)
                   (map #(str/split % #" "))
                   (parse))]
  (println "x" (eval-circuit "x" circuit))
  (println "y" (eval-circuit "y" circuit))
  (println "d" (eval-circuit "d" circuit))
  (println "e" (eval-circuit "e" circuit))
  (println "f" (eval-circuit "f" circuit))
  (println "g" (eval-circuit "g" circuit))
  (println "h" (eval-circuit "h" circuit))
  (println "i" (eval-circuit "i" circuit)))

(let [circuit (->> (slurp "day7.txt")
                   (str/split-lines)
                   (map #(str/split % #" "))
                   (parse))]
  (println (eval-circuit "a" circuit)))
