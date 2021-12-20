(load-file "util.clj")

(def mat (parse-matrix (read-lines "day11input.txt")))

(def all-coords (for [x (range (count (first mat)))
                      y (range (count mat))]
                  [x y]))

(defn get-adjacent-coords [m x y]
  (filter (fn [[xn yn]] (not (nil? (get-pt m xn yn))))
          [[(inc x) y] [x (inc y)]
           [(dec x) y] [x (dec y)]
           [(inc x) (inc y)] [(dec x) (dec y)]
           [(dec x) (inc y)] [(inc x) (dec y)]]))

(defn inc-all [m] (mapv #(mapv inc %) m))

(defn flash [m [x y]]
  (reduce (fn [mn [xn yn]] (replace-matrix-elem mn xn yn (inc (get-pt mn xn yn))))
          m
          (get-adjacent-coords m x y)))

(defn run-flashes
  ([m] (run-flashes m 0 #{}))
  ([m num-flashes has-flashed]
   (let [to-flash (filter (fn [[x y]] (and (not (has-flashed [x y]))
                                           (> (get-pt m x y) 9)))
                          all-coords)]
     (if (empty? to-flash)
       [m num-flashes]
       (recur (reduce flash m to-flash)
              (+ num-flashes (count to-flash))
              (apply conj has-flashed to-flash))))))

(defn decrease-energy [m]
  (mapv (fn [row] (mapv #(if (> % 9) 0 %) row)) m))

(defn step [m]
  (let [inced (inc-all m)
        [flashed num-flashes] (run-flashes inced)
        new-m (decrease-energy flashed)]
    [new-m num-flashes]))

(defn do-steps [m] (iterate (fn [[mn _]] (step mn)) [m 0]))

(defn count-flashes [m num-steps]
  (->> (do-steps m)
       (take (inc num-steps))
       (map second)
       (apply +)))

(defn find-sync-step [m]
  (let [flash-counts (map second (do-steps m))
        m-size (* (count m) (count (first m)))]
    (first (keep-indexed (fn [i n] (when (= n m-size) i)) flash-counts))))
