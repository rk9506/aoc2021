(load-file "util.clj")

(def lines (read-lines "day10input.txt"))

(def opening #{\( \[ \{ \<})
(def match-opening {\( \), \[ \], \{ \}, \< \>})
(def corrupt-scores {\) 3 \] 57 \} 1197 \> 25137})
(def complete-scores {\) 1 \] 2 \} 3 \> 4})

(defn parse
  ([line] (parse line '()))
  ([line stack]
   (let [c (first line)]
     (cond
       (nil? c) [0 stack]
       (match-opening c) (parse (rest line)
                                (conj stack (match-opening c)))
       (not= c (first stack)) [(corrupt-scores c) stack] 
       :else (parse (rest line) (rest stack))))))

(defn get-score [line] (first (parse line)))

(defn part-1 [] (apply + (map get-score lines)))

(defn get-completion-score [s]
  (reduce (fn [score c] (+ (* score 5) (complete-scores c))) 0 s))

(defn middle [l] (nth l (dec (/ (inc (count l)) 2))))

(defn part-2 []
  (->> lines
       (map parse)
       (filter #(zero? (first %)))
       (map second)
       (map get-completion-score)
       sort
       middle))
