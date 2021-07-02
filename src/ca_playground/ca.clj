(ns ca-playground.ca
  (:require [ca-playground.grid :as grid]))

(defn moore-neighborhood
  "Returns the Moore neighborhood of the cell at the given coordinates. The Moore neighborhood of a
  cell in the grid consists of the 8 adjacent cells. The Moore neighborhood is represented as a
  flattened seq of values read from the grid in a left-to-right, top-to-bottom manner.

  This function treats border cells from one end of the grid as neighbors to borders cells from the
  other end, resulting in a toroidal topoplogy."
  [grid row col]
  (let [[width height] (grid/dims grid)
        mod-coords     (fn [[row col]]
                         [(mod row width)
                          (mod col height)])
        neigh-coords   [[(dec row) (dec col)]
                        [(dec row) col]
                        [(dec row) (inc col)]
                        [row (dec col)]
                        [row (inc col)]
                        [(inc row) (dec col)]
                        [(inc row) col]
                        [(inc row) (inc col)]]]
    (map (fn [coords]
           (grid/gget grid (mod-coords coords)))
         neigh-coords)))

(defn update-grid
  "Update each cell in the grid using update-fn. update-fn is invoked with two arguments: the value of
  the current cell and the moore-neighborhood, represented as a flat seq."
  [grid update-fn]
  (reduce (fn [new-grid [x y v]]
            (let [nh (moore-neighborhood grid x y)]
              (grid/gassoc new-grid
                [x y] (update-fn v nh))))
          grid
          grid))

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
