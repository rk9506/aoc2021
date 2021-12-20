(load-file "util.clj")

(def mat (parse-matrix (read-lines "day15input.txt")))

(defn get-all-coords [m]
  (for [x (range (count (first m)))
        y (range (count m))]
    [x y]))

(defn find-closest-vertex [vs dists]
  (apply min-key #(if (contains? dists %) (dists %) Integer/MAX_VALUE) vs))

(defn get-adjacent-coords [m [x y]]
  (filter (fn [[xn yn]] (not (nil? (get-pt m xn yn))))
          [[(inc x) y] [x (inc y)]
           [(dec x) y] [x (dec y)]]))

(defn update [vs dists prevs u m]
  (if (empty? vs)
    [dists prevs]
    (let [v (first vs)
          d (+ (dists u) (get-pt m (first v) (second v)))]
      (if (or (nil? (dists v)) (< d (dists v)))
        (recur (rest vs) (assoc dists v d) (assoc prevs v u) u m)
        (recur (rest vs) dists prevs u m)))))

(defn calc-distances [unvisited dists prevs target m]
  (if (empty? unvisited)
    [dists prevs]
    (let [u (find-closest-vertex unvisited dists)
          new-unvisited (disj unvisited u)
          vs (filter unvisited (get-adjacent-coords m u))
          [new-dists new-prevs] (update vs dists prevs u m)]
      (if (= u target)
        [dists prevs]
        (recur new-unvisited new-dists new-prevs target m)))))

(defn shortest-path
  ([target prevs] (shortest-path prevs '()))
  ([target prevs path]
   (if (nil? (prevs target))
     path
     (recur (prevs target) prevs (conj path target)))))

(defn shortest-dist [m]
  (let [target [(dec (count (first m))) (dec (count m))]
        [dists prevs] (calc-distances (set (get-all-coords m))
                                      {[0 0] 0}
                                      {}
                                      target
                                      m)]
    (dists target)))

(defn part-1 [] (shortest-dist mat))

(defn add-to-row [row to-add]
  (mapv #(let [result (+ % to-add)]
           (if (= result 9) 9 (mod result 9)))
        row))

(defn expand-row [row]
  (mapcat #(add-to-row row %) (range 5)))

(defn expand-rows [rows]
  (mapcat (fn [to-add]
            (mapv (fn [row] (add-to-row row to-add)) rows))
          (range 5)))

(defn expand-map [m]
  (into [] (expand-rows (mapv expand-row m))))

(defn part-2 [] (shortest-dist (expand-map mat)))

