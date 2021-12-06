(defn read-lines [filename]
  (clojure.string/split-lines (slurp filename)))

(defn count-occurences [needle haystack]
  (count (filter #(= % needle) haystack)))

(defn find-index [pred coll]
  (first (keep-indexed #(when (pred %2) %1) coll)))
