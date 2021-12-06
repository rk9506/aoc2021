(load-file "util.clj")

(def input (mapv #(Integer/parseInt %) (clojure.string/split (first (read-lines "day6input.txt")) #",")))

(defn step-naive [timers]
  (let [num-new (count (filter zero? timers))
        updated-timers (mapv #(if (zero? %) 6 (dec %)) timers)]
    (concat updated-timers (repeat num-new 8))))

(defn simulate-naive [timers days] (nth (iterate step timers) days))
(defn part-1 [] (count (simulate input 80)))

(defn step-fast [counts]
  (let [num-new (first counts)
        new-sixth-count (+ (nth counts 7) num-new)
        updated (assoc (subvec counts 1) 6 new-sixth-count)]
    (conj updated num-new)))

(defn get-counts [timers]
  (reduce (fn [v t] (assoc v t (inc (nth v t)))) (into [] (repeat 9 0)) timers))

(defn simulate-fast [timers days] (nth (iterate step-fast (get-counts timers)) days))
(defn part-2 [] (apply + (simulate-fast input 256)))
