(ns ca-playground.ca
  (:require [ca-playground.grid :as grid]))

(defn mod-coords
  [[width height] [row col]]
  [(mod row width)
   (mod col height)])

(defn moore-neighborhood-with-wrap
  [[row col] grid-dims]
  (mapv (partial mod-coords grid-dims)
        [[(dec row) (dec col)]
         [(dec row) col]
         [(dec row) (inc col)]
         [row (dec col)]
         [row (inc col)]
         [(inc row) (dec col)]
         [(inc row) col]
         [(inc row) (inc col)]]))

;; TODO: Can producing dups and then deduping be avoided?
;; TODO: is there a better name for this function? Maybe
;;       there's a name for the neighbors of a list of coordinates?
(defn potential-updates
  "Given the dimensions of the grid, the list of coordinates that were updated at the previous time
  step, and the neighbordhood function that is in use, returns a comprehensive list of coordinates
  that could potentially have updates at the next time step."
  [grid-dims prev-update-coords neighborhood-fn]
  (set (mapcat (fn [coord]
                 (neighborhood-fn coord grid-dims))
               prev-update-coords)))

(defn grid-updates
  "Return the list of updates for the next timestep for the CA according to update-fn. Optionally, for
  improved performance, takes a seq of prev-update-coords: the comprehensive list of coordinates
  that were updated at the previous timestep."
  ([grid update-fn prev-update-coords]
   (reduce (fn [updates coord]
             (let [value     (grid/gget grid coord)
                   neighbors (map (partial grid/gget grid)
                                  (moore-neighborhood-with-wrap coord (grid/dims grid)))
                   new-state (update-fn value neighbors)]
               (if (not= value new-state)
                 (conj updates [coord new-state])
                 updates)))
           nil
           (potential-updates (grid/dims grid) prev-update-coords moore-neighborhood-with-wrap)))
  ([grid update-fn]
   (grid-updates grid update-fn (map butlast grid))))

(defn game-of-life-update-fn
  [value neighborhood]
  (assert (= 8 (count neighborhood)))
  (let [n-live (apply + neighborhood)]
    (condp = [value n-live]
      [1 2] 1
      [1 3] 1
      [0 3] 1
      0)))

(defn highlife-update-fn
  [value neighborhood]
  (assert (= 8 (count neighborhood)))
  (let [n-live (apply + neighborhood)]
    (condp = [value n-live]
      [1 2] 1
      [1 3] 1
      [0 3] 1
      [0 6] 1
      0)))
