(load-file "util.clj")

(def input (mapv #(Integer/parseInt %) (clojure.string/split (first (read-lines "day7input.txt")) #",")))

(defn cost-part-1 [crab target] (Math/abs (- crab target)))

(defn get-fuel-costs [cost-fn]
  (let [candidates (range (apply min input) (inc (apply max input)))]
    (map (fn [p] (apply + (map #(cost-fn p %) input))) candidates)))

(defn get-best-cost [cost-fn] (apply min (get-fuel-costs cost-fn)))

(defn part-1 [] (get-best-cost cost-part-1))

(defn cost-part-2 [crab target]
  (let [num-steps (inc (cost-part-1 crab target))]
    (* 0.5 num-steps (- num-steps 1))))

(defn part-2 [] (get-best-cost cost-part-2))
