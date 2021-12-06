(load-file "util.clj")

(def depths (into [] (map #(Integer/parseInt %) (read-lines "day1input.txt"))))

(defn count-increases [ns]
  (apply + (map #(if (> (nth ns %) (nth ns (dec %))) 1 0)
                (range 1 (count ns)))))

(println (count-increases depths))

(def sliding-window-sums
  (map #(apply + (subvec depths % (+ 3 %)))
       (range 0 (- (count depths) 2))))

(println (count-increases sliding-window-sums))

