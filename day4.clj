(def input (clojure.string/split (slurp "day4input.txt") #"\n\n"))
(def drawn-nums (map #(Integer/parseInt %)
                     (clojure.string/split (first input) #",")))
(defn parse-board [txt]
  (mapv (fn [line]
          (mapv (fn [n] {:val (Integer/parseInt n) :marked false})
                (filter not-empty (clojure.string/split line #" "))))
        (clojure.string/split-lines txt)))

(def boards (map parse-board (rest input)))

(defn mark-board [n board]
  (mapv (fn [line]
          (mapv (fn [entry] (if (= (:val entry) n)
                              (assoc entry :marked true)
                              entry))
                line))
        board))

(defn draw-num [n boards]
  (mapv #(mark-board n %) boards))

(defn has-complete-row? [board]
  (some #(every? :marked %) board))

(defn won? [board]
  (or (has-complete-row? board)
      (has-complete-row? (apply mapv vector board))))

(defn get-score [board n]
  (* n (apply + (->> (flatten board)
                   (filter #(not (:marked %)))
                   (map :val)))))

(defn play-part-1 [nums boards]
  (let [num (first nums)
        new-boards (draw-num num boards)
        winners (filter won? new-boards)]
    (if (empty? winners)
      (play-part-1 (rest nums) new-boards)
      (get-score (first winners) num))))

(play-part-1 drawn-nums boards)

(defn play-part-2 [nums boards]
  (let [num (first nums)
        new-boards (draw-num (first nums) boards)
        losers (filter #(not (won? %)) new-boards)]
    (if (= (count losers) 1)
      (play-part-1 (rest nums) losers)
      (play-part-2 (rest nums) losers))))

(play-part-2 drawn-nums boards)
