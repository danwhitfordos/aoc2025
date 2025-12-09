(require '[utils]
         '[clojure.string :as str])

(def test-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defn make-range [s]
  (let [[from to] (str/split s #"[-]")]
    (range (Long/parseLong from)
           (inc (Long/parseLong to)))))

(defn parse-ids [input]
  (let [range-strs (str/split input #"[,]")]
    (map make-range range-strs)))

(defn invalid? [n]
  (let [id (Long/toString n)
        l (count id)
        a (subs id 0 (/ l 2))
        b (subs id (/ l 2))]
    ;; (assert (zero? (mod l 2)) id)
    (cond
      (= (mod l 2) 1) false
      (= a b) true
      :else false)))

(let [ids (parse-ids test-input)
      invalids (mapcat #(filter invalid? %) ids)]
  (apply + invalids))

(def my-input "4077-5314,527473787-527596071,709-872,2487-3128,6522872-6618473,69137-81535,7276-8396,93812865-93928569,283900-352379,72-83,7373727756-7373754121,41389868-41438993,5757-6921,85-102,2-16,205918-243465,842786811-842935210,578553879-578609405,9881643-10095708,771165-985774,592441-692926,7427694-7538897,977-1245,44435414-44469747,74184149-74342346,433590-529427,19061209-19292668,531980-562808,34094-40289,4148369957-4148478173,67705780-67877150,20-42,8501-10229,1423280262-1423531012,1926-2452,85940-109708,293-351,53-71")

(let [ids (parse-ids my-input)
      invalids (mapcat #(filter invalid? %) ids)]
  (apply + invalids))

;; wrong  8959930376
;; part 1 13108371860

;; part 2

(defn invalid2? [n]
  (let [id (Long/toString n)]
    (re-matches #"([0-9]+)\1{1,}" id)))

(let [ids (parse-ids test-input)
      invalids (mapcat #(filter invalid2? %) ids)]
  (println invalids)
  (apply + invalids))

(defn add-up-range [r]
   (let [filtered (filter invalid2? r)]
    (reduce + filtered)))

(time (let [ids (parse-ids my-input)
            invalids (pmap add-up-range ids)]
        ;; (println invalids)
        (reduce + invalids)))

;; right 22471660255