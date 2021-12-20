(def input (slurp "day14input.txt"))

(def start (first (clojure.string/split-lines input)))

(defn parse-rules [s]
  (let [matches (mapv rest (re-seq #"([^\s]+) -> ([^\s])" s))]
    (into {} (map (fn [[k v]] {k v}) matches))))

(def rules (parse-rules input))

(defn pairs [s]
  (map #(str (nth s %) (nth s (inc %)))
       (range (dec (count s)))))

(defn get-or-zero [m k] (if (m k) (m k) 0))

(defn apply-rules
  ([pair-counts char-counts rules] (apply-rules {} pair-counts char-counts rules))
  ([new-pair-counts old-pair-counts char-counts rules]
   (if (empty? old-pair-counts)
     [new-pair-counts char-counts]
     (let [[pair n] (first old-pair-counts)
           match (first (rules pair))
           p1 (str (first pair) match)
           p2 (str match (second pair))]
       (if (nil? match)
         (recur (assoc new-pair-counts pair n) (dissoc old-pair-counts pair) char-counts rules)
         (recur (assoc new-pair-counts
                       p1 (+ (get-or-zero new-pair-counts p1) n)
                       p2 (+ (get-or-zero new-pair-counts p2) n))
                (dissoc old-pair-counts pair)
                (assoc char-counts match (+ (get-or-zero char-counts match) n))
                rules))))))

(defn iterate-growth [s rules]
  (iterate (fn [[pair-counts char-counts]] (apply-rules pair-counts char-counts rules))
           [(frequencies (pairs s)) (frequencies s)]))

(defn get-range [s rules n]
  (-> (iterate-growth start rules)
      (nth n)
      second
      vals
      (#(- (apply max %) (apply min %)))))

(defn part-1 [] (get-range start rules 10))
(defn part-2 [] (get-range start rules 40))
