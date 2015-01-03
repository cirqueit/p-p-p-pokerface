(ns p-p-p-pokerface)

(def rank-values {"T" 10 "J" 11 "Q" 12 "K" 13 "A" 14})

(defn rank [card]
  (let [[f _] card
        fst (str f)]
    (if (Character/isDigit f)
      (Integer/valueOf fst)
      (rank-values fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
   (= 1 (count (filter #(= 2 %) freqs)))))

(defn three-of-a-kind? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
   (= 1 (count (filter #(= 3 %) freqs)))))

(defn four-of-a-kind? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
   (= 1 (count (filter #(= 4 %) freqs)))))

(defn flush? [hand]
  (let [freqs (vals (frequencies (map suit hand)))]
   (= 1 (count (filter #(= 5 %) freqs)))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
   (= 2 (count (filter #(= 2 %) freqs)))))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        low (first sorted-hand)
        straight (range low (+ 5 low))]
    (or (= sorted-hand straight) (= sorted-hand [2 3 4 5 14]))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (and (not (flush? hand)) (= 1 (apply max freqs)))))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter #((first %) hand) checkers)))))
