(defn read-lines [filename]
  (clojure.string/split-lines (slurp filename)))

(defn count-occurences [needle haystack]
  (count (filter #(= % needle) haystack)))

(defn find-index [pred coll]
  (first (keep-indexed #(when (pred %2) %1) coll)))

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn parse-row [row] (mapv #(Character/digit % 10) row))

(defn parse-matrix [lines] (mapv parse-row lines))

(defn get-pt [hm x y]
  (if (or (< x 0)
          (< y 0)
          (= x (count (first hm)))
          (= y (count hm)))
    nil
    ((hm y) x)))

(defn replace-matrix-elem [m x y e]
  (assoc m y (assoc (m y) x e)))
