(def lines (read-lines "day12input.txt"))

(defn parse-line [l]
  (let [[from to] (clojure.string/split l #"-")]
    [[from to] [to from]]))

(defn group-transitions [ts]
  (into {} (map (fn [[from vals]] {from (map second vals)})
                (group-by first ts))))

(def transitions (group-transitions (mapcat parse-line lines)))

(defn small-cave? [s] (not= s (clojure.string/upper-case s)))

(defn paths-from [c cave-map small-seen seen-counts can-visit?]
  (let [new-seen (assoc small-seen c (if (small-seen c) (inc (small-seen c)) 1))
        new-seen-counts (if (small-cave? c) (conj seen-counts (new-seen c)) seen-counts)
        to-visit (filter #(can-visit? % new-seen new-seen-counts) (cave-map c))]
    (cond (= c "end") `((~c))
          :else (map #(conj % c)
                     (mapcat #(paths-from % cave-map new-seen new-seen-counts can-visit?)
                             to-visit)))))

(defn can-visit-part-1 [c small-seen seen-counts]
  (or (not (small-cave? c)) (nil? (small-seen c))))

(defn part-1 []
  (count (paths-from "start" transitions {} #{} can-visit-part-1)))

(defn can-visit-part-2 [c small-seen seen-counts]
  (and (not (and (= c "start") (= (small-seen c) 1)))
       (not (and (= c "end") (= (small-seen c) 1)))
       (or (not (small-cave? c))
           (nil? (small-seen c))
           (and (= (small-seen c) 1)
                (not (seen-counts 2))))))

(defn part-2 []
  (count (paths-from "start" transitions {} #{} can-visit-part-2)))
