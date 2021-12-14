(load-file "util.clj")
(def lines (read-lines "day8input.txt"))

(defn split-space [s] (clojure.string/split s #" "))

(defn parse-note [line]
  (let [parts (clojure.string/split line #" \| ")
        signal-strings (split-space (first parts))
        output-strings (split-space (second parts))]
    {:signals (map #(apply vector %) signal-strings) :output output-strings}))

(def notes (map parse-note lines))

(def num-segs-per-digit [6 2 5 5 4 5 6 3 7 6])
(def num-occs-per-seg {\a 8 \b 6 \c 8 \d 7 \e 4 \f 9 \g 7})
(def segs-to-digit {#{\a \b \c \e \f \g} \0
                    #{\c \f} \1
                    #{\a \c \d \e \g} \2
                    #{\a \c \d \f \g} \3
                    #{\b \c \d \f} \4
                    #{\a \b \d \f \g} \5
                    #{\a \b \d \e \f \g} \6
                    #{\a \c \f} \7
                    #{\a \b \c \d \e \f \g} \8
                    #{\a \b \c \d \f \g} \9})

(defn part-1 []
  (let [seg-counts (set (map num-segs-per-digit [1 4 7 8]))
        outputs (flatten (map :output notes))]
    (count (filter #(seg-counts (count %)) outputs))))

(defn diff [x y] (clojure.set/difference (set x) (set y)))

(defn find-other [freqs n to-eliminate]
  (first (find-first (fn [[key val]] (and (= val n)
                                          (not= key to-eliminate)))
                     freqs)))

(defn get-signal [signals digit]
  (find-first #(= (count %) (num-segs-per-digit digit)) signals))

(defn get-mappings [signals]
  (let [freqs (frequencies (flatten signals))
        char-by-freq (clojure.set/map-invert freqs)
        one-signal (get-signal signals 1)
        seven-signal (get-signal signals 7)
        four-signal (get-signal signals 4)
        b-mapped (char-by-freq (num-occs-per-seg \b))
        e-mapped (char-by-freq (num-occs-per-seg \e))
        f-mapped (char-by-freq (num-occs-per-seg \f))
        a-mapped (first (diff seven-signal one-signal))
        c-mapped (find-other freqs 8 a-mapped)
        d-mapped (find-first #(and (not= % b-mapped)
                                   (not= % c-mapped)
                                   (not= % f-mapped))
                             four-signal)
        g-mapped (find-other freqs 7 d-mapped)]
    {a-mapped \a b-mapped \b c-mapped \c d-mapped \d e-mapped \e f-mapped \f g-mapped \g}))

(defn get-output-value [note]
  (let [mappings (get-mappings (note :signals))
        mapped-output (map #(map mappings %) (note :output))
        output-digits (map #(segs-to-digit (set %)) mapped-output)]
    (Integer/parseInt (apply str output-digits))))

(defn part-2 []
  (apply + (map get-output-value notes)))
