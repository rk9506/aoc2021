(declare explode)
(declare find-greater-than)
(declare split)

(defn find-nested
  ([x n pred f] (or (find-nested x n pred f [] 0) (find-nested x n pred f [] 1)))
  ([x n pred f c i]
   (let [new-c (conj c i)
         el (get-in x new-c)]
     (if (pred el n)
       new-c
       (if (coll? el)
         (or (find-nested x (f n) pred f new-c 0)
             (find-nested x (f n) pred f new-c 1))
         nil)))))

(defn find-to-explode [x]
  (find-nested x 4 (fn [el n] (and (= n 1) (coll? el))) dec))

(defn find-to-split [x]
  (find-nested x 4 (fn [el _] (and (number? el) (>= el 10))) identity))

(defn replace-last [p n m]
  (let [i-reversed (find-index #(= % n) (reverse p))]
    (if i-reversed
      (let [i (- (count p) i-reversed 1)]
        (take (inc i) (assoc p i m))
        nil))))

(defn add-to-el [x p n]
  (assoc-in x p (+ (get-in x p) n)))

(defn explode
  ([x] (explode x (find-to-explode x)))
  ([x p]
   (let [[n m] (get-in x p)
         left-p (replace-last p 1 0)
         right-p-cand (replace-last p 0 1)
         right-p (if (or (nil? right-p-cand) (number? (get-in x right-p-cand)))
                   right-p-cand
                   (conj right-p-cand 0))
         a (println right-p right-p-cand)
         left-replaced (if left-p (add-to-el x left-p n) x)
         right-replaced (if right-p (add-to-el left-replaced right-p m) left-replaced)]
     (assoc-in right-replaced p 0))))

(defn reduce-num [x]
  (let [to-explode (find-to-explode x)]
    (if to-explode
      (reduce-num (explode x to-explode))
      (let [to-split (find-greater-than x 10)]
        (reduce-num (split x to-split))
        x))))

(defn add-nums [x y]
  (reduce-num [x y]))
