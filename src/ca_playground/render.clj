(ns ca-playground.render
  (:require
   [ca-playground.ca :as ca]
   [ca-playground.grid :as grid]
   [quil.core :as q]
   [quil.middleware :as m])
  (:import
   [processing.core PConstants]))

(def ca-configs
  [{:name         "Game of Life"
    :cell-states  [{:color [255 255 255]}
                   {:color [0 0 0]}]
    :rule-fn      ca/game-of-life-update-fn}
   {:name        "Highlife"
    :cell-states [{:color [255 255 255]}
                  {:color [0 0 0]}]
    :rule-fn     ca/highlife-update-fn}])

(defn init-state
  []
  (let [init-grid (grid/vec-grid-of (repeat 0) 100 100)]
    {:status     :paused
     :ca-configs ca-configs
     :curr-ca    0
     :curr-grid  init-grid
     :grid-coll  nil}))

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

(def key-defs
  [{:key  :space
    :desc "run/pause"
    :fn   (fn [state _]    
            (case (:status state)
              :paused  (let [grid      (:curr-grid state)
                             update-fn (get-in state [:ca-configs
                                                      (:curr-ca state)
                                                      :rule-fn])
                             grid-coll (iterate #(ca/update-grid % update-fn)
                                                grid)]
                         (assoc state
                           :status    :running
                           :curr-grid grid
                           :grid-coll grid-coll))
              :running (assoc state
                         :status :paused
                         :grid-coll nil)))}
   {:key  :r
    :desc "reset"
    :fn   (fn [state _]
            (assoc (init-state) :curr-ca (:curr-ca state)))}
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
   {:key  :R
    :desc "randomize"
    :fn   (fn [state _]
            (if (= :paused (:status state))
              (let [[width height] (grid/dims (:curr-grid state))
                    n-states       (-> state
                                       (get-in [:ca-configs
                                                (:curr-ca state)
                                                :cell-states])
                                       count)]
                (assoc (init-state)
                  :curr-grid (random-ca width height n-states)))
              state))}])

(defn setup []
  (q/frame-rate 15)
  (q/background 0)
  (init-state))

(defn mouse-event
  "Cycle the state of the clicked cell."
  [state event]
  (let [{:keys [status
                ca-configs
                curr-ca
                curr-grid]} state]
    (if (= status :paused)
      (let [[rows cols] (grid/dims curr-grid)
            cell-height (/ (q/height) rows)
            cell-width  (/ (q/width)  cols)
            x           (int (Math/floor (/ (:x event) cell-width)))
            y           (int (Math/floor (/ (:y event) cell-height)))
            curr-val    (grid/gget curr-grid [x y])
            curr-ca     (get ca-configs curr-ca)]
        (update state
                :curr-grid
                #(grid/gassoc %
                   [x y] (if (< curr-val (dec (count (:cell-states curr-ca))))
                           (inc curr-val)
                           0))))
      state)))

(defn key-pressed
  [state event]
  (let [key-map (->> key-defs
                     (map (juxt :key identity))
                     (into {}))
        f (get-in key-map [(:key event) :fn])]
    (if f (f state event) state)))

(defn update-state
  [{:keys [status grid-coll] :as state}]
  (case status
    :paused state
    :running (-> (assoc state :curr-grid (first grid-coll))
                 (update :grid-coll next))))

(defn draw-key-help
  [x y]
  (let [[key-str desc-str]
        (reduce (fn [[key-str desc-str] key-def]
                  [(str key-str (:key key-def) "\n")
                   (str desc-str (:desc key-def) "\n")])
                ["" ""]
                key-defs)]
    (q/text-size 16)
    (q/text "KEYS\n" x y)
    (q/text key-str x (+ y 25))
    (q/text desc-str (+ x 100) (+ y 25))))

(defn draw
  [state]
  (q/clear)
  (let [{:keys [status
                ca-configs
                curr-ca
                curr-grid]} state
        ca                  (get ca-configs curr-ca)
        [rows cols]         (grid/dims curr-grid)
        cell-height         (/ (q/height) rows)
        cell-width          (/ (q/width) cols)
        shape-group         (.createShape (q/current-graphics) PConstants/GROUP)]
    (doseq [row (range rows)
            col (range cols)]
      (let [color (get-in ca
                          [:cell-states (grid/gget curr-grid [row col]) :color])
            x     (* row cell-width) 
            y     (* col cell-height)
            shape (.createShape (q/current-graphics) PConstants/RECT (into-array Float/TYPE [(float x) (float y) (float cell-width) (float cell-height)]))
            ]
        (q/fill color)
        (.setFill shape (.color (q/current-graphics) (first color) (second color) (last color)))
        (.setStrokeWeight shape 2)
        (.addChild shape-group shape)))
    (q/shape shape-group)
    (q/fill 120)
    (q/stroke 120)
    (q/stroke-weight 10)
    (q/text-size 16)
    (q/text (format "%s. Click around to toggle cell state." (:name ca)) 20 20)
    (q/text (format "Current frame rate: %s" (q/current-frame-rate)) 20 40)
    (q/text (str "Status: " (name status)) (- (q/width) 150) 20)
    (draw-key-help 20 (- (q/height) 150))))

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
