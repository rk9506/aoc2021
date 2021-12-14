(load-file "util.clj")

(def lines (read-lines "day9input.txt"))

(def heightmap (parse-matrix lines))

(defn true-for-all-adjacent? [pred hm x y])

(defn low-point? [hm x y]
  (let [p (get-pt hm x y)
        max-x (dec (count (first hm)))
        max-y (dec (count hm))]
    (and (or (zero? x) (< p (get-pt hm (dec x) y)))
         (or (zero? y) (< p (get-pt hm x (dec y))))
         (or (= x max-x) (< p (get-pt hm (inc x) y)))
         (or (= y max-y) (< p (get-pt hm x (inc y)))))))

(defn low-points [hm]
  (let [x-dim (count (first hm))
        y-dim (count hm)
        all-points (for [x (range x-dim) y (range y-dim)] [x y])]
    (filter (fn [[x y]] (low-point? hm x y)) all-points)))

(defn part-1 []
  (apply + (map (fn [[x y]] (inc (get-pt heightmap x y)))
                (low-points heightmap))))

(defn search-basin [hm init-stack]
  (defn next-stack-and-basin [basin stack]
    (let [curr-entry (first stack)
          prev-val (:prev-val curr-entry)
          [x y] (:point curr-entry)
          curr-val (get-pt hm x y)]
      (if (or (nil? curr-val)
              (= curr-val 9)
              (<= curr-val prev-val))
        [basin (rest stack)]
        [(conj basin [x y]) (conj (rest stack)
                                  {:prev-val curr-val :point [(dec x) y]}
                                  {:prev-val curr-val :point [x (dec y)]}
                                  {:prev-val curr-val :point [(inc x) y]}
                                  {:prev-val curr-val :point [x (inc y)]})])))
  (loop [basin #{}
         stack init-stack]
    (if (empty? stack)
      basin
      (let [[next-basin next-stack] (next-stack-and-basin basin stack)]
        (recur next-basin next-stack)))))

(defn get-basin [hm x y]
  (search-basin hm (list {:prev-val (dec (get-pt hm x y)) :point [x y]})))

(defn part-2 []
  (->> (low-points heightmap)
       (map (fn [[x y]] (get-basin heightmap x y)))
       (map count)
       (sort >)
       (take 3)
       (apply *)))
