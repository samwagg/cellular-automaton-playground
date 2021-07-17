(ns ca-playground.grid
  (:require
    [clojure.string :as string]))

(defprotocol Grid
  "An abstraction of 2D grids. A more performant implementation of a grid is likely to be needed in
  the future, so an abstraction seemed prudent.

  Grids are somewhat like an associative data structure like Clojure's Vector and Map, where row,col
  coordinates act as keys. They support some analogous functions (e.g. gget, gassoc).

  Coordinates are 2 element vectors of the form [num-rows num-columns]."
  (dims [grid]
    "Returns the dimensions of the grid in the form [n-rows n-cols]")
  (gget [grid coord]
    "Returns the value at the given coordinate.")
  (gassoc [grid coord x]
    "Returns a new grid where the value at the given coordinate is replaced by x.")
  (subgrid [grid min-coord max-coord]
    "Returns a new grid that is a subgrid of grid. min-coord and max-coord can be thought of as the
    upper left corner and the bottom right corner of the subgrid."))

(deftype VecGrid [vec2d]
  Grid
  (dims [_] [(count vec2d) (count (first vec2d))])
  (gget [_ coord]
    (let [[row col] coord]
      (get-in vec2d [row col]))) 
  (gassoc [_ coord val] (let [[x y] coord]
                          (assert (boolean (get-in vec2d coord))
                                  "Coordinate must be within grid.")
                          (VecGrid. (assoc-in vec2d [x y] val))))
  (subgrid [this min-coord max-coord]
    (let [[row-min col-min] min-coord
          [row-max col-max] max-coord
          [rows cols] (dims this)]
      (VecGrid. (vec (for [i (range row-min (inc row-max))
                           :while (< i rows)] 
                       (vec (for [j (range col-min (inc col-max))
                                  :while (< j cols)]
                              (gget this [i j]))))))))
  clojure.lang.Seqable
  (seq [this]
    (let [[_ cols] (dims this)]
      (->> (apply concat vec2d)
           (map-indexed (fn [i item] [(int (/ i cols))
                                      (mod i cols)
                                      item])))))

  (equals [this o]
    (and (= VecGrid (type o))
         (= (seq this) (seq o))
         (= (dims this) (dims o))))
  (hashCode [_]
    (hash vec2d))
  (toString [_] (str (->> (map #(map pr-str %) vec2d)
                          (map #(str "|" (string/join " " %) "|"))
                          (string/join "\n"))
                     "\n")))

(defmethod print-method VecGrid [grid ^java.io.Writer w]
  (.write w (.toString grid)))

(defn vec-grid
  "Construct a VecGrid from vec2d, a vector of vectors where each inner vector represents a row in a
  grid.

  Example

  (vec-grid-of [[1 2 3]
                [4 5 6]])
  =>
  1 2 3
  4 5 6"
  [vec2d]
  (assert (or (empty? vec2d)
              (apply = (map count vec2d)))
          "grid rows must all be the same length")
  (->VecGrid vec2d))

(defn vec-grid-of
  "Construct a VecGrid from the values in coll. The values from coll are partitioned into the rows of
  a grid of the specified width and height. coll must contain at least width * height values, but
  can contain more (extra values are discarded).  

  Example:
  
  (vec-grid-of (range) 3 3)
  =>
  0 1 2
  3 4 5
  6 7 8"
  [coll width height]
  (let [n-cells (* width height)]
    ;; Can't use count directly to check that coll is large enough because coll can be
    ;; infinite.
    (assert (= n-cells (count (take n-cells coll)))
            "coll must have at least (* width height) elements"))
  (->> (partition width coll)
       (map vec)
       (take height)
       vec
       vec-grid))
