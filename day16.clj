(declare parse-multiple)
(def version-len 3)
(def type-id-len 3)
(def header-len (+ version-len type-id-len))
(def length-type-id-pos 6)
(def length-len 15)
(def num-subpackets-len 11)
(def subpacket-length-start (+ header-len length-len 1))
(def subpacket-num-start (+ header-len num-subpackets-len 1))
(def val-seg-len 5)

(defn with-binary-output [p] (fn [x y] (if (p x y) 1 0)))

(def ops {0 +, 1 *, 2 min, 3 max, 5 (with-binary-output >) , 6 (with-binary-output <) , 7 (with-binary-output =)})

(defn to-zero-padded-binary [n]
  (clojure.string/replace (format "%4s" (Integer/toBinaryString n)) " " "0"))

(defn hex->binary [s]
  (apply str (map #(to-zero-padded-binary (Character/digit % 16)) s)))

(defn parse-version [s]
  (Integer/parseInt (subs s 0 3) 2))

(defn decode-value
  ([s] (decode-value s ""))
  ([s acc]
   (let [next-acc (str acc (subs s 1 val-seg-len))]
     (if (= (first s) \0)
       [(Long/parseLong next-acc 2) (subs s val-seg-len)]
       (recur (subs s val-seg-len) next-acc)))))

(defn parse-literal [s]
  (let [[value remainder] (decode-value (subs s header-len))]
    [{:version (parse-version s)
      :type :literal
      :value value}
     remainder]))

(defn parse-subpacket-len [s]
  (Integer/parseInt (subs s (inc header-len) subpacket-length-start) 2))

(defn parse-num-subpackets [s]
  (Integer/parseInt (subs s (inc header-len) subpacket-num-start) 2))

(defn parse-type-id [s] (Integer/parseInt (subs s 3 6) 2))

(defn parse-op [s]
  (let [length-type-id (nth s header-len)
        [subpackets remainder] (if (= length-type-id \0)
                                 [(parse-multiple
                                   (subs s
                                         subpacket-length-start
                                         (+ subpacket-length-start (parse-subpacket-len s))))
                                  (subs s (+ subpacket-length-start (parse-subpacket-len s)))]
                                 (parse-multiple (subs s subpacket-num-start) (parse-num-subpackets s)))]
    [{:version (parse-version s)
      :type :op
      :op-func (ops (parse-type-id s))
      :subpackets subpackets}
     remainder]))

(defn parse [s]
  (let [type-id (parse-type-id s)]
    (if (= type-id 4)
      (parse-literal s)
      (parse-op s))))

(defn parse-multiple
  ([s]
   (let [[packet remainder] (parse s)]
     (if (empty? remainder)
       (list packet)
       (let [packets (parse-multiple remainder)]
         (conj packets packet)))))
  ([s n]
   (let [[packet remainder] (parse s)]
     (if (= n 1)
       [(list packet) remainder]
       (let [[packets final-remainder] (parse-multiple remainder (dec n))]
         [(conj packets packet) final-remainder])))))

(def parse-hex (comp parse hex->binary))

(defmulti version-sum :type)
(defmethod version-sum :literal [packet] (:version packet))
(defmethod version-sum :op [packet]
  (apply + (:version packet) (map version-sum (:subpackets packet))))

(defn part-1 []
  (version-sum (first (parse-hex (slurp "day16input.txt")))))

(defmulti calc :type)
(defmethod calc :literal [packet] (:value packet))
(defmethod calc :op [packet]
  (apply (:op-func packet) (map calc (:subpackets packet))))

(defn part-2 [] (calc (first (parse-hex (slurp "day16input.txt")))))
