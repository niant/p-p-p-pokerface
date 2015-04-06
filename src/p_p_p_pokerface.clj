(ns p-p-p-pokerface)

(def ranks {\2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9, \T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card]
    (ranks rank)))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn same-ranks? [frequency-count hand]
  (let [ranks (map rank hand)
        rank-counts (vals (frequencies ranks))]
    (<= frequency-count (apply max rank-counts))))

(defn pair? [hand]
  (same-ranks? 2 hand))

(defn three-of-a-kind? [hand]
  (same-ranks? 3 hand))

(defn four-of-a-kind? [hand]
  (same-ranks? 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= 1 (count (set suits)))))

(defn hand-rank-count? [rank-count hand]
  (let [ranks (map rank hand)
        rank-counts (vals (frequencies ranks))]
    (= rank-count (sort rank-counts))))

(defn full-house? [hand]
  (hand-rank-count? [2 3] hand))

(defn two-pairs? [hand]
  (hand-rank-count? [1 2 2] hand))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks-low-ace (replace {14 1} ranks)
        smallest (apply min ranks)
        smallest-low-ace (apply min ranks-low-ace)
        needed-range (range smallest (+ 5 smallest))
        needed-range-low-ace (range smallest-low-ace (+ 5 smallest-low-ace))]
    (or (= needed-range (sort ranks))
        (= needed-range-low-ace (sort ranks-low-ace)))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        get-score (fn [[check-fn points]] (if (check-fn hand) points 0))]
    (apply max (map get-score checkers))))
