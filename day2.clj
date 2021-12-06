(load-file "util.clj")

(defn parse-inst [line]
  (let [pair (clojure.string/split line #" ")]
    [(keyword (first pair)) (Integer/parseInt (second pair))]))

(def insts (map parse-inst (read-lines "day2input.txt")))

(defn get-pos-p1 [insts hor-pos depth]
  (if (empty? insts)
    [hor-pos depth]
    (let [inst (first insts)
          op (first inst)
          val (second inst)]
      (cond (= op :forward) (get-pos (rest insts) (+ hor-pos val) depth)
            (= op :down) (get-pos (rest insts) hor-pos (+ depth val))
            (= op :up) (get-pos (rest insts) hor-pos (- depth val))))))

(println (apply * (get-pos-p1 insts 0 0)))

(defn get-pos-p2 [insts hor-pos depth aim]
  (if (empty? insts)
    [hor-pos depth]
    (let [inst (first insts)
          op (first inst)
          val (second inst)]
      (cond (= op :forward) (get-pos-p2 (rest insts) (+ hor-pos val) (+ depth (* aim val)) aim)
            (= op :down) (get-pos-p2 (rest insts) hor-pos depth (+ aim val))
            (= op :up) (get-pos-p2 (rest insts) hor-pos depth (- aim val))))))

(println (apply * (get-pos-p2 insts 0 0 0)))
