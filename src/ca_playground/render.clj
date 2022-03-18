(ns ca-playground.render
  (:require
   [ca-playground.ca :as ca]
   [ca-playground.grid :as grid]
   [quil.core :as q]
   [quil.middleware :as m])
  (:import
   [processing.core PConstants]))

(def init-frame-rate 15)

(def ca-configs
  [{:name         "Game of Life"
    :cell-states  [{:color [255 255 255]}
                   {:color [0 0 0]}]
    :rule-fn      ca/game-of-life-update-fn}
   {:name        "Highlife"
    :cell-states [{:color [255 255 255]}
                  {:color [0 0 0]}]
    :rule-fn     ca/highlife-update-fn}])

(defn init-grid
  [rows cols]
  (grid/vec-grid-of (repeat 0) rows cols))

(defn init-state
  []
  {:target-frame-rate init-frame-rate
   :status     :paused
   :ca-configs ca-configs
   :curr-ca    0
   :curr-grid  (init-grid 50 50)
   :curr-updates nil
   :grid-coll  nil
   :overlay-visible? true})

(defn random-ca
  [width height n-states]
  (grid/vec-grid-of (repeatedly #(rand-int n-states))
                    width
                    height))

;; Eager generation of a seq of successive grid updates. Useful for performance testing the
;; animation.
#_(def coll
  (->> (random-ca 100 100 2)
       (iterate #(ca/update-grid % ca/game-of-life-update-fn))
       (take 1000)
       doall))

(defn supergrid
  [grid n-rows n-cols default-val]
  (grid/vec-grid-of
    (for [row (range n-rows)
          col (range n-cols)]
      (if-let [val (grid/gget grid [row col])]
        val
        default-val))
    n-cols
    n-rows))

(defn gen-ca-coll
  [init-grid update-fn]
  (iterate (fn [[grid prev-updates]]
             (let [updates  (if prev-updates
                              (ca/grid-updates grid update-fn (map first prev-updates))
                              (ca/grid-updates grid update-fn))
                   new-grid (reduce (fn [new-grid [[row col] new-state]]
                                      (grid/gassoc new-grid
                                                   [row col] new-state))
                                    grid
                                    updates)]
               [new-grid updates]))
           [init-grid nil]))

(defn curr-ca-fn
  [state]
  (get-in state [:ca-configs
                 (:curr-ca state)
                 :rule-fn]))

(defn reset
  [state]
  (-> (init-state)
      (merge (select-keys state [:curr-ca :target-frame-rate :overlay-visible?]))
      (assoc :curr-grid (apply init-grid (grid/dims (:curr-grid state))))))

(def key-defs
  [{:key  :space
    :desc "run/pause"
    :fn   (fn [state _]    
            (case (:status state)
              :paused  (let [grid      (:curr-grid state)
                             update-fn (curr-ca-fn state)
                             grid-coll (gen-ca-coll grid update-fn)]
                         (assoc state
                           :status    :running
                           :curr-grid grid
                           :curr-updates nil
                           :grid-coll grid-coll))
              :running (assoc state
                         :status :paused
                         :grid-coll nil)))}
   {:key  :right
    :desc "next CA"
    :fn   (fn [state _]
           (if (= :paused (:status state))
             (if (< (:curr-ca state) (dec (count (:ca-configs state))))
               (update state :curr-ca inc)
               (assoc state :curr-ca 0))
             state))}
   {:key  :left
    :desc "previous CA"
    :fn   (fn [state _]
           (if (= :paused (:status state))
             (if (= 0 (:curr-ca state))
               (assoc state :curr-ca (dec (count (:ca-configs state))))
               (update state :curr-ca dec))
             state))}
   {:key :up
    :desc "increase framerate"
    :fn   (fn [state _]
            (update state :target-frame-rate inc))}
   {:key :down
    :desc "decrease framerate"
    :fn   (fn [state _]
            (if (< 1 (:target-frame-rate state))
              (update state :target-frame-rate dec)
              state))}
   {:key :+
    :desc "increase grid size"
    :fn    (fn [state _]
             (let [{:keys [curr-grid]} state
                   [rows cols]         (grid/dims curr-grid)
                   grid                (supergrid curr-grid (inc rows) (inc cols) 0)]
               (assoc state
                 :curr-updates nil
                 :curr-grid grid
                 :grid-coll (gen-ca-coll grid (curr-ca-fn state)))))}
   {:key  :-
    :desc "decrease grid size"
    :fn   (fn [state _]
            (let [{:keys [curr-grid]} state
                  [rows cols]         (grid/dims curr-grid)
                  grid                (grid/subgrid curr-grid [0 0] [(- rows 2) (- cols 2)])]
              (assoc state
                :curr-updates nil
                :curr-grid grid
                :grid-coll (gen-ca-coll grid (curr-ca-fn state)))))}
   {:key  :r
    :desc "reset"
    :fn   (fn [state _] (reset state))}
   {:key  :R
    :desc "randomize"
    :fn   (fn [state _]
            (let [state          (reset state)
                  [width height] (grid/dims (:curr-grid state))
                  n-states       (-> state
                                     (get-in [:ca-configs
                                              (:curr-ca state)
                                              :cell-states])
                                     count)]
              (assoc state
                :curr-grid (random-ca width height n-states)
                :curr-updates nil)))}
   {:key  :h
    :desc "Hide overlays"
    :fn   (fn [state _]
            (-> (assoc state :overlay-update-counter 2)
                (update :overlay-visible? not)))}])

(defn setup []
  (q/frame-rate init-frame-rate)
  (q/background 0)
  (init-state))

(defn mouse-event
  "Cycle the state of the clicked cell."
  [state event]
  (let [{:keys [status
                ca-configs
                curr-ca
                curr-grid
                curr-updates]} state]
    (if (= status :paused)
      (let [[rows cols] (grid/dims curr-grid)
            cell-height (/ (q/height) rows)
            cell-width  (/ (q/width)  cols)
            x           (int (Math/floor (/ (:x event) cell-width)))
            y           (int (Math/floor (/ (:y event) cell-height)))
            curr-val    (grid/gget curr-grid [x y])
            curr-ca     (get ca-configs curr-ca)
            new-state   (if (< curr-val (dec (count (:cell-states curr-ca))))
                          (inc curr-val)
                          0)
            ;; If the same cell is clicked twice, replace the existing entry for that cell rather
            ;; than appending a new one.
            update-map  (into {} curr-updates)
            updates'    (seq (assoc update-map [x y] new-state))]
        (-> (update state :curr-grid #(grid/gassoc % [x y] new-state))
            (assoc :curr-updates updates')))
      state)))

(defn key-pressed
  [state event]
  (let [key-map (->> key-defs
                     (map (juxt :key identity))
                     (into {}))
        f (get-in key-map [(:key event) :fn])]
    (if f (f state event) state)))

(defn update-state
  [{:keys [status grid-coll overlay-visible? prev-overlay-visible? redraw?] :as state}]
  (let [state' (cond-> state
                 redraw? (assoc :redraw? false)
                 (and (not overlay-visible?)
                      prev-overlay-visible?) (assoc :redraw? true)
                 true (assoc :prev-overlay-visible? overlay-visible?))]
    (case status
      :paused  state'
      :running (let [[grid updates] (first grid-coll)]
                 (-> state'
                     (assoc :curr-grid grid
                            :curr-updates updates)
                     (update :grid-coll next)
                 )))))

(defn draw-header
  [status ca-name grid-size]
  (q/stroke-weight 0)
  (q/fill 0)
  (q/rect 0 0 (q/width) 70)
  (q/fill 255)
  (q/text-size 16)
  (q/text (format "%s. Click around to toggle cell state." ca-name) 20 20)
  (let [frame-rate-text
        (cond-> (format "Target frame rate: %s." (q/target-frame-rate))
          (= status :running) (str (format " Actual frame rate %s" (q/current-frame-rate))))]
    (q/text frame-rate-text 20 40))
  (q/text (format "Grid size: %s X %s" (first grid-size) (second grid-size))
          20 60)
  (q/text (str "Status: " status) (- (q/width) 150) 20))

(defn draw-key-help
  [x y]
  (let [[key-str desc-str]
        (reduce (fn [[key-str desc-str] key-def]
                  [(str key-str (:key key-def) "\n")
                   (str desc-str (:desc key-def) "\n")])
                ["" ""]
                key-defs)]
    (q/stroke-weight 0)
    (q/text-size 12)
    (q/fill 0)
    (q/rect x (- y 20) 250 250)
    (q/fill 255)
    (q/text "KEYS\n" (+ x 15) y)
    (q/text key-str (+ x 15) (+ y 15))
    (q/text desc-str (+ x 125) (+ y 15))))

(defn draw-fresh-grid
  [grid colors]
  (q/clear)
  (let [[rows cols] (grid/dims grid)
        cell-height (/ (q/height) rows)
        cell-width  (/ (q/width) cols)
        shape-group (.createShape (q/current-graphics) PConstants/GROUP)]
    (doseq [row (range rows)
            col (range cols)]
      (let [color (get colors (grid/gget grid [row col]))
            x     (* row cell-width) 
            y     (* col cell-height)
            shape (.createShape (q/current-graphics) PConstants/RECT (into-array Float/TYPE [(float x) (float y) (float cell-width) (float cell-height)]))
            ]
        (q/fill color)
        (.setFill shape (.color (q/current-graphics) (first color) (second color) (last color)))
        (.setStroke shape (q/color 0 0 0))
        (.setStrokeWeight shape 1)
        (.addChild shape-group shape)))
    (q/shape shape-group)))

(defn draw-grid-updates
  [grid-dims updates colors]
  (let [[rows cols] grid-dims
        cell-height (/ (q/height) rows)
        cell-width  (/ (q/width) cols)
        shape-group (.createShape (q/current-graphics) PConstants/GROUP)]
    (doseq [[coord state] updates]
      (let [color     (get colors state)
            [row col] coord
            x         (* row cell-width) 
            y         (* col cell-height)
            shape     (.createShape (q/current-graphics) PConstants/RECT (into-array Float/TYPE [(float x) (float y) (float cell-width) (float cell-height)]))]
        (q/fill color)
        (.setFill shape (.color (q/current-graphics) (first color) (second color) (last color)))
        (.setStroke shape (q/color 0 0 0))
        (.setStrokeWeight shape 1)
        (.addChild shape-group shape)))
    (q/shape shape-group)))

(defn draw
  [state]
  (q/stroke 120)
  (let [{:keys [status
                target-frame-rate
                ca-configs
                curr-ca
                curr-grid
                curr-updates
                redraw?
                overlay-visible?]} state
        ca                  (get ca-configs curr-ca)
        colors              (mapv :color (:cell-states ca))]
    (q/frame-rate target-frame-rate)
    (if (and curr-updates (not redraw?))
      (draw-grid-updates (grid/dims curr-grid) curr-updates colors)
      (draw-fresh-grid curr-grid colors))
    (when overlay-visible?
      (draw-header status (:name ca) (grid/dims curr-grid))
      (draw-key-help 0 (- (q/height) 190)))))

(q/defsketch ca
  :title "CA Playground"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :mouse-pressed mouse-event
  :key-pressed key-pressed
  :update update-state
  :size [800 800]
  :middleware [m/fun-mode])
