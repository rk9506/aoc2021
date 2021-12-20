(def input (slurp "day13input.txt"))

(def dots (->> (re-seq #"(\d+),(\d+)" input)
               (map rest)
               (map (fn [l] (map #(Integer/parseInt %) l)))))

(defn find-lines [s]
  (->> (re-seq #"(x|y)=(\d+)" s)
       (map rest)
       (map (fn [[axis n]] {:axis axis :value (Integer/parseInt n)}))))

(defn reflect-coord [coord line]
  (if (<= coord line)
    coord
    (- (* 2 line) coord)))

(defn reflect-y [[x y] l] [x (reflect-coord y l)])
(defn reflect-x [[x y] l] [(reflect-coord x l) y])

(defn reflect [p {:keys [axis value]}]
  (if (= axis "x") (reflect-x p value) (reflect-y p value)))

(defn part-1 []
  (count (set (map #(reflect % (first (find-lines input))) dots))))

(defn dot-matrix [dots]
  (let [max-x (apply max (map first dots))
        max-y (apply max (map second dots))
        dots-set (set dots)]
    (for [i (range (inc max-x))]
      (for [j (range (inc max-y))]
        (if (dots-set [i j]) "*" " ")))))

(defn part-2 []
  (->> (reduce (fn [m line] (map #(reflect % line) m))
               dots
               (find-lines input))
       set
       dot-matrix
       reverse
       (map #(str (apply str %) "\n"))
       (apply str)
       println))

