(load-file "util.clj")

(def lines (map (fn [s] (map #(Character/getNumericValue %) s))
                (read-lines "day3input.txt")))
(def col-sums (apply map + lines))
(defn bin-list->int [l]
  (Integer/parseInt (apply str l) 2))
(defn get-diagnostic [pred]
  (->> col-sums
       (map #(if (pred %) 1 0))
       (bin-list->int)))

(def gamma (get-diagnostic #(>= % (/ (count lines) 2))))
(def epsilon (get-diagnostic #(< % (/ (count lines) 2))))

(println (* gamma epsilon))

(defn life-support-rating [nums pred i]
  (if (= (count nums) 1)
    (first nums)
    (let [total (apply + (map #(nth % i) nums))
          keep (if (pred total (/ (count nums) 2)) 1 0)]
      (life-support-rating (filter #(= (nth % i) keep) nums)
                           pred
                           (inc i)))))

(defn to-bin-list [n]
  (map #(Character/getNumericValue %)
       (Integer/toBinaryString n)))
(defn part-2 []
  (let [gamma-list (to-bin-list gamma)
        epsilon-list (to-bin-list epsilon)
        oxygen-gen (life-support-rating lines >= 0)
        co2-scrub (life-support-rating lines < 0)]
    (* (bin-list->int oxygen-gen)
       (bin-list->int co2-scrub))))

(part-2)
