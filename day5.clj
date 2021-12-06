(load-file "util.clj")
(def lines (read-lines "day5input.txt"))

(defn parse-coord [s]
  (map #(Integer/parseInt %) (clojure.string/split s #",")))

(defn parse-seg [line]
  (let [coords (map parse-coord (clojure.string/split line #"\s->\s"))]
    {:from (first coords) :to (second coords)}))

(def segs (mapv parse-seg lines))

(defn non-diag? [seg]
  (or (= (first (seg :from)) (first (seg :to)))
      (= (second (seg :from)) (second (seg :to)))))

(def non-diag-segs (filter non-diag? segs))

(defn seg->points [seg]
  (let [[x1 y1] (seg :from)
        [x2 y2] (seg :to)
        steps (inc (max (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))
        dx (compare x2 x1)
        dy (compare y2 y1)]
    (mapv #(list (+ x1 (* dx %))
                 (+ y1 (* dy %)))
          (range 0 steps))))

(def size (inc (apply max (flatten (map #(vector (first (% :to)) (second (% :to))) segs)))))
(def grid (into [] (repeat size (into [] (repeat size 0)))))

(defn plot-point [grid point]
  (let [x (first point)
        y (second point)]
    (assoc grid y (assoc (grid y) x
                         (inc ((grid y) x))))))

(defn plot-seg [grid seg]
  (loop [points (seg->points seg)
         modified-grid grid]
    (if (empty? points)
      modified-grid
      (recur (rest points)
             (plot-point modified-grid (first points))))))

(defn plot-all [grid segs]
  (reduce plot-seg grid segs))

(defn count-at-least-two-overlapping [grid]
  (->> grid
       (flatten)
       (filter #(>= % 2))
       (count)))

(defn part-1 [] (count-at-least-two-overlapping (plot-all grid non-diag-segs)))
(defn part-2 [] (count-at-least-two-overlapping (plot-all grid segs)))
