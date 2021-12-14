(load-file "util.clj")

(def mat (parse-matrix (read-lines "day11test.txt")))

(def all-coords (for [x (range (count (first mat)))
                      y (range (count mat))]
                  [x y]))

(defn get-adjacent-coords [m x y]
  (filter (fn [xn yn] (not (nil? (get-pt m xn yn))))
          [[(inc x) y] [x (inc y)]
           [(dec x) y] [x (dec y)]
           [(inc x) (inc y)] [(dec x) (dec y)]]))

(defn inc-all [m] (mapv #(mapv inc %) m))

(defn flash [m [x y]]
  (reduce (fn [mn [xn yn]] (replace-matrix-elem mn xn yn (inc (get-pt xn yn))))
          m
          (get-adjacent-coords m x y)))

(defn run-flashes [m]
  (let [to-flash (filter #(> (get-pt %) 9) all-coords)]
    (if (empty? to-flash)
      m
      (run-flashes (reduce flash m to-flash)))))

(defn set-flashed-to-)

(defn step [m]
  (let [inced ]))
