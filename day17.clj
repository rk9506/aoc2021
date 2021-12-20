(def input (slurp "day17input.txt"))

(defn parse-input [s]
  (let [areas (map #(Integer/parseInt %) (re-seq #"-?\d+" s))]
    {:x-min (first areas)
     :x-max (second areas)
     :y-min (nth areas 2)
     :y-max (nth areas 3)}))

(defn step [[x y] [dx dy]]
  (let [xp (+ x dx)
        yp (+ y dy)]
    [[xp yp]
     [(+ dx (cond (zero? dx) 0
                  (pos? dx) -1
                  :else 1))
      (dec dy)]]))

(defn in-target? [[x y] {:keys [x-min x-max y-min y-max]}]
  (and (>= x x-min)
       (<= x x-max)
       (>= y y-min)
       (<= y y-max)))

(defn overshot? [[x y] {:keys [y-min]}]
  (or (< y y-min)))

(defn max-y-if-hit
  ([v target] (max-y-if-hit [0 0] v '(0) target))
  ([p v ys target]
   (cond (overshot? p target) nil
         (in-target? p target) (apply max ys)
         :else (let [[new-p new-v] (step p v)]
                 (recur new-p new-v (conj ys (second new-p)) target)))))

(defn find-max-ys [target]
  (let [to-try (for [dx (range 1 (inc (target :x-max)))
                     dy (range (target :y-min) (+ (target :x-min) (target :x-max) 1))]
                 [dx dy])]
    (filter #(not (nil? %)) (map #(max-y-if-hit % target) to-try))))

(defn part-1 [] (apply max (find-max-ys (parse-input input))))
(defn part-2 [] (count (find-max-ys (parse-input input))))
