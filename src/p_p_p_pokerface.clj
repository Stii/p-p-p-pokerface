(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card
        rnk_map {\T 10
                 \J 11
                 \Q 12
                 \K 13
                 \A 14}]
    (if (Character/isDigit (char rnk))
      (Integer/valueOf (str rnk))
      (if (contains? rnk_map rnk)
        (get rnk_map rnk)
        ))))

(defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn rank_freq [hand]
  (let [rank_map (map rank hand)
        freq (vals (frequencies rank_map))]
    freq))

(defn pair? [hand]
  (let [freq (rank_freq hand)]
    (= (apply max freq) 2)
    ))

(defn three-of-a-kind? [hand]
  (let [freq (rank_freq hand)]
    (= (apply max freq) 3)
    ))

(defn four-of-a-kind? [hand]
  (let [freq (rank_freq hand)]
    (= (apply max freq) 4)
    ))

(defn flush? [hand]
  (let [suit_map (map suit hand)
        freq (vals (frequencies suit_map))]
    (= (apply max freq) 5)))

(defn full-house? [hand]
  (let [freq (rank_freq hand)]
    (and
      (= (apply min freq) 2)
      (= (apply max freq) 3))))

(defn two-pairs? [hand]
  (let [freq (rank_freq hand)
        pairs (frequencies freq)]
    (= (get pairs 2) 2)))

(defn straight? [hand]
  (let [rank_map (map rank hand)
        sorted-hand (sort rank_map)
        lower (first sorted-hand)
        upper (last sorted-hand)]
    (if (and (= lower 2) (= upper 14))
      (= (sort (replace {14 1} (seq sorted-hand))) (range 1 6))
      (= sorted-hand (range lower (+ upper 1))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        check (fn [checker] (if ((first checker) hand) (second checker)))]
    (apply max (filter (fn [v] (not (nil? v))) (map check checkers)))))
