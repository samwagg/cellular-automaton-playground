(ns ca-playground.grid
  (:require
    [clojure.string :as string]))

(defprotocol Grid
  "An abstraction of 2D grids. A more performant implementation of a grid is likely to be needed in
  the future, so an abstraction via protocol seemed prudent.

  Grids are somewhat like an associative data structure like Clojure's Vector and Map, where x,y
  coordinates act as keys. They support some analogous functions (e.g. gget, gassoc)."
  (dims [grid])
  ;; 'grid get', 'grid assoc', etc.
  (gget [grid coord])
  (gassoc [grid coord x])
  (subgrid [grid min-coord max-coord]))

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
  (hashCode [this]
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
