(require '[utils]
         '[clojure.string :as str]
         '[clojure.data.priority-map :as pm])

(def test-input "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

(defn parse-state [[hd & rst]]
  (cond
    (= hd \[) (parse-state rst)
    (= hd \]) nil
    (= hd \.) (cons 0 (parse-state rst))
    (= hd \#) (cons 1 (parse-state rst))))

(defn parse-button [buttons]
  (map Integer/parseInt (str/split
                         (subs buttons 1 (dec (count buttons)))
                         #",")))

(defn parse-buttons [buttons]
  (map parse-button buttons))

(defn parse-machine [line]
  (let [bits (str/split line #" ")
        state (parse-state (first bits))
        buttons (parse-buttons (drop-last (rest bits)))]
    {:target-state state :nlights (count state) :buttons buttons}))

(defn parse [input]
  (map parse-machine (str/split-lines input)))

(defn score [state target]
  (count (filter true? (map not= state target))))

(score '(0 0 0 0 0) '(0 1 0 1 0))
(score '(0 0 0 0 0) '(1 1 1 1 1))

(defn apply-button [current button]
  (map-indexed #(if (contains? (set button) %1) (bit-xor %2 1) %2) current))

(apply-button '(0 0 0 0 0) '(3))
(apply-button '(0 0 0 0 0) '(0 2))

(defn next-steps [current target buttons visited]
  (->> (map
        #(let [next-state (apply-button (:state current) %)
               next-path (conj (:path current) next-state)]
           [(merge current {:state next-state} {:path next-path})
            (+ (count next-path))])
        buttons)
       (filter #(not (contains? visited (:state (first %)))))))

(next-steps {:state '(0 0 0 0) :path []} '(0 1 1 0) '((3) (1 3) (2) (2 3) (0 2) (0 1)) #{})
(next-steps {:state '(0 0 0 0) :path []} '(0 1 1 0) '((3) (1 3) (2) (2 3) (0 2) (0 1)) #{'(1 1 0 0)})

(defn shortest-path [pq machine visited]
  (let [[[curr curr-score] & rst] pq
        target (:target-state machine)
        buttons (:buttons machine)]
    (assert (contains? curr :state))
    (assert (not (nil? curr)))
    (cond
      (= (:state curr) target) (:path curr)
      :else (let [next (next-steps curr target buttons visited)]
              (shortest-path
               (into (pop pq) next)
               machine
               (into visited (map (fn [[node _]] (:state node))) next))))))

(defn proc-machine [machine]
  (println "Processing" machine)
  (let [start-state (take (:nlights machine) (repeat 0))
        target-state (:target-state machine)]
    ;; (shortest-path-tail
    (shortest-path
     (pm/priority-map {:state start-state :path []} (score start-state target-state))
     machine
     #{})))

(proc-machine {:target-state '(0 1 1 0), :nlights 4, :buttons '((3) (1 3) (2) (2 3) (0 2) (0 1))})

(map proc-machine (parse test-input))

(let [machines (parse test-input)]
  (assert (= (transduce
              (comp
               (map proc-machine)
               (map count))
              +
              machines) 7)))

(let [machines (parse (slurp "day10.txt"))]
  (transduce
   (comp
    (map proc-machine)
    (map count))
   +
   machines))

;; part 2

(defn parse-joltage [bit]
  (parse-button bit))

(defn parse-machine2 [line]
  (let [bits (str/split line #" ")
        buttons (parse-buttons (drop-last (rest bits)))
        joltage (parse-joltage (last bits))]
    {:target-joltage joltage
     :buttons buttons}))

(defn parse2 [input]
  (map parse-machine2 (str/split-lines input)))

(parse2 test-input)

(defn apply-joltage [current button]
  (map-indexed #(if (contains? (set button) %1) (inc %2) %2) current))

(apply-joltage '(0 0 0 0) '(0 2))
(apply-joltage '(0 1 2 3) '(1 3))

(defn distance [current machine]
  (apply + (map #(abs (- %2 %1)) (:joltage current) (:target-joltage machine))))

(distance {:joltage '(0 0 0 0)} {:target-joltage '(2 3 4 5)})
(distance {:joltage '(2 3 2 2)} {:target-joltage '(2 3 4 5)})
(distance {:joltage '(2 3 4 5)} {:target-joltage '(2 3 4 5)})

(defn not-dead-end? [current machine]
  (every? true? (map #(<= %1 %2) (:joltage current) (:target-joltage machine))))

(not-dead-end? {:joltage '(0 0 0 0)} {:target-joltage '(2 3 4 5)})
(not-dead-end? {:joltage '(2 3 4 5)} {:target-joltage '(2 3 4 5)})
(not-dead-end? {:joltage '(2 4 4 5)} {:target-joltage '(2 3 4 5)})

(defn next-steps2 [current buttons visited machine]
  (let [next (map
              #(let [next-path (inc (:pathl current))
                     next-joltage (apply-joltage (:joltage current) %)]
                 [(merge current {:pathl next-path} {:joltage next-joltage})
                  (+ next-path (distance {:joltage next-joltage} machine))])
              buttons)
        filtered (filter #(not (contains? visited (select-keys (first %) [:joltage]))) next)
        dead (filter #(not-dead-end? (first %) machine) filtered)]
    ;; (when (not= (count next) (count filtered)) (println "|||"))
    ;; (when (not= (count filtered) (count dead)) (println "XXX"))
    dead))

;; (next-steps2 {:state '(0 0 0 0) :path [] :joltage '(0 0 0 0)}
;;              '((3) (1 3) (2) (2 3) (0 2) (0 1)) #{})
;; (next-steps2 {:state '(0 0 0 0) :path [] :joltage '(1 1 1 1)}
;;              '((3) (1 3) (2) (2 3) (0 2) (0 1)) #{'(1 1 0 0)})

(defn shortest-path2 [pq machine visited]
  (loop [pq pq visited visited]
    (let [[[curr cscore] & _] pq
          target-joltage (:target-joltage machine)
          buttons (:buttons machine)]
      (assert (not (nil? curr)))
      (assert (contains? curr :joltage))
      ;; (println cscore "|" curr)
      (cond
        (= (:joltage curr) target-joltage)
        (:pathl curr)
        :else (let [next (next-steps2 curr buttons visited machine)]
                (recur
                 (into (pop pq) next)
                 (into visited (map (fn [[node _]]
                                      (select-keys node [:joltage]))) next)))))))

(defn proc-machine2 [machine]
  (println "Processing" machine)
  (let [start-joltage (take (count (:target-joltage machine)) (repeat 0))]
    (shortest-path2
     (pm/priority-map {:joltage start-joltage :pathl 0} 0)
     machine
     #{})))

(proc-machine2 {:target-joltage '(3 5 4 7)
                :buttons '((3) (1 3) (2) (2 3) (0 2) (0 1))})

(map proc-machine2 (parse2 test-input))

(let [machines (parse2 test-input)]
  (assert (= (transduce
              (map proc-machine2)
              +
              machines) 33)))

(let [machines (parse2 (slurp "day10.txt"))]
  (transduce
   (map proc-machine2)
   +
   machines))
