(import (javax.swing JFrame JPanel SwingUtilities Timer)
        (java.awt BorderLayout Color)
        (java.awt.event MouseAdapter ActionListener))

(require 'clojure.set)

(defmacro trace [& body]
  `(do
     (prn ~body)
     ~body))

(def +window-width+ 1024)
(def +window-height+ 768)
(def +gauge-left+ 300)
(def +gauge-top+ 10)
(def +gauge-width+ 9)
(def +gauge-height+ 15)
(def +cell-size+ 50)
(def +colors+ [Color/BLACK Color/BLUE Color/ORANGE Color/YELLOW Color/MAGENTA])
(def +dirs+ [[0 1] [1 0] [0 -1] [-1 0]])
(def +shrinking-counter-max+ 5)
(def +falling-counter-max+ 5)

(defn random-color []
  (inc (rand-int (dec (count +colors+)))))

(defn random-seq [size]
  (map (fn [x] (random-color)) (range size)))

(defn random-gauge []
  (vec (random-seq (* +gauge-width+ +gauge-height+))))

(def +initial-game-state+
  {:gauge (random-gauge)
   :shrinking-cells {}
   :shrinking-counter 0
   :falling-cells {}
   :falling-counter 0})


(defn index-2d-to-1d [[x y]]
  (+ x (* y +gauge-width+)))

(defn index-1d-to-2d [index]
  [(rem index +gauge-width+) (quot index +gauge-width+)])

(defn pos-add [pos1 pos2]
  (vec (map + pos1 pos2)))

(defn pos-valid? [[x y]]
  (and (>= x 0) (< x +gauge-width+)
       (>= y 0) (< y +gauge-height+)))

(defn direct-neighbors [index]
  (let [pos (index-1d-to-2d index)
        neighbor-poses (map #(pos-add % pos) +dirs+)
        neighbors (filter pos-valid? neighbor-poses)
        neighbor-indices (map index-2d-to-1d neighbors)]
    (set neighbor-indices)))

(defn connected-neighbors [index gauge]
  (letfn [(search [open-list closed-list]
            (if (empty? open-list)
              closed-list
              (let [x (first open-list)
                    neighbors (direct-neighbors x)
                    candidates (set (filter #(= (gauge x) (gauge %)) neighbors))
                    existing (clojure.set/union open-list closed-list)
                    new-cells (clojure.set/difference candidates existing)]
                (recur (clojure.set/union new-cells (disj open-list x))
                       (conj closed-list x)))))]
    (search #{index} #{})))

(defn screen-coordinates [[x y]]
  [(+ +gauge-left+ (* x +cell-size+))
   (+ +gauge-top+ (* y +cell-size+))])

(defn get-color [color-index]
  (get +colors+ color-index (first +colors+)))
  
(defn draw-gauge [g gauge]
  (doseq [x (range +gauge-width+)
          y (range +gauge-height+)]
    (let [[left top] (screen-coordinates [x y])
          color-index (get gauge (index-2d-to-1d [x y]))]
      (doto g
        (.setColor (get-color color-index))
        (.fillRect left top (dec +cell-size+) (dec +cell-size+))))))

(defn draw-shrinking [g cells counter]
  (doseq [[index color-index] cells]
    (let [pos (index-1d-to-2d index)
          [left top] (screen-coordinates pos)
          offset (int (* +cell-size+ (- 1 (/ counter +shrinking-counter-max+))))
          small-cell-size (- +cell-size+ (* 2 offset))]
      (doto g
        (.setColor (get-color color-index))
        (.fillRect (+ left offset) (+ top offset)
                   small-cell-size small-cell-size)))))

(defn draw-falling [g cells counter]
  (doseq [[index color-index] cells]
    (let [pos (index-1d-to-2d index)
          [left top] (screen-coordinates pos)
          offset (int (* +cell-size+ (/ counter +falling-counter-max+)))]
      (doto g
        (.setColor (get-color color-index))
        (.fillRect left (- top offset) (dec +cell-size+) (dec +cell-size+))))))

(defn draw-game-state [g gs]
  (let [{:keys [gauge shrinking-cells shrinking-counter
                falling-cells falling-counter]} gs]
    (draw-gauge g gauge)
    (when-not (empty? shrinking-cells)
      (draw-shrinking g shrinking-cells shrinking-counter))
    (when-not (empty? falling-cells)
      (draw-falling g falling-cells falling-counter))))

(defn get-index-from-mouse-event [e]
  (let [mouse-x (.getX e)
        mouse-y (.getY e)
        x (quot (- mouse-x +gauge-left+) +cell-size+)
        y (quot (- mouse-y +gauge-top+) +cell-size+)]
    (if (pos-valid? [x y])
      (index-2d-to-1d [x y])
      nil)))

(defn clear-gauge-for [to-be-removed gauge]
  (reduce #(assoc %1 %2 0) gauge to-be-removed))

(defn get-gauge-2d [gauge x y]
  (get gauge (index-2d-to-1d [x y])))

(defn get-falling-cells-on-col [gauge x]
  (let [holes-y (filter #(= 0 (get-gauge-2d gauge x %)) (range +gauge-height+))]
    (if (empty? holes-y)
      {}
      (let [lowest-hole-y (reduce max holes-y)
            cells-hash (for [y (range 0 lowest-hole-y)]
                         (let [old-index (index-2d-to-1d [x y])
                               new-index (index-2d-to-1d [x (inc y)])
                               color (get gauge old-index)]
                           {new-index color}))
            cells (reduce merge {(index-2d-to-1d [x 0]) (random-color)}
                          cells-hash)]
        cells))))

(defn get-falling-cells [gauge]
  (reduce merge (for [x (range +gauge-width+)]
                  (get-falling-cells-on-col gauge x))))

(defn merge-cells-to-gauge [gauge cells]
  (reduce #(assoc %1 (key %2) (val %2)) gauge cells))  

(defn has-hole [gauge]
  (some zero? gauge))

(defn no-solution? [gauge]
  (every? #(< (count (connected-neighbors % gauge)) 3) (range (count gauge))))

(defn rescue [gauge]
  (let [x (rand-int (- +gauge-width+ 2))
        index-of-x (index-2d-to-1d [x (dec +gauge-height+)])
        color-index (get gauge index-of-x)
        cells (for [offset (range 3)]
                (let [index (index-2d-to-1d [(+ x offset) (dec +gauge-height+)])]
                  {index color-index}))
        cells-hash (reduce merge cells)]
    (merge-cells-to-gauge gauge cells-hash)))
    
(defn ensure-gauge-solvable [gauge]
  (if (no-solution? gauge)
    (rescue gauge)
    gauge))

(defn ensure-gs-solvable [gs]
  (update-in gs [:gauge] ensure-gauge-solvable))

(defn game-state-ready? [gs]
  (and (empty? (:falling-cells gs)) (empty? (:shrinking-cells gs))))

(defn game-state-remove-cells [gs neighbors]
  (let [gauge (:gauge gs)]
    {:gauge (clear-gauge-for neighbors gauge)
     :shrinking-cells (zipmap neighbors (map gauge neighbors))
     :shrinking-counter +shrinking-counter-max+
     :falling-cells {}
     :falling-counter 0}))

(defn game-state-on-click [gs index]
  (let [neighbors (connected-neighbors index (:gauge gs))]
    (if (> (count neighbors) 2)
      (game-state-remove-cells gs neighbors)
      gs)))

(defn game-state-shrink [gs]
  (let [{:keys [gauge shrinking-counter shrinking-cells]} gs]
    (if (> shrinking-counter 0)
      (update-in gs [:shrinking-counter] dec)
      (merge gs {:shrinking-cells {}
                 :shrinking-counter 0
                 :falling-cells (get-falling-cells gauge)
                 :falling-counter +falling-counter-max+}))))

(defn game-state-fall [gs]
  (let [{:keys [gauge falling-counter falling-cells]} gs]
    (if (> falling-counter 0)
      (update-in gs [:falling-counter] dec)
      (let [new-gauge (merge-cells-to-gauge gauge falling-cells)]
        (if (has-hole new-gauge)
          (merge gs {:gauge new-gauge
                     :falling-cells (get-falling-cells new-gauge)
                     :falling-counter +falling-counter-max+})
          (merge gs {:gauge new-gauge
                     :falling-cells {}
                     :falling-counter 0}))))))

(defn game-state-on-tick [gs]
  (let [{:keys [gauge shrinking-cells falling-cells]} gs]
    (cond (not (empty? shrinking-cells)) (game-state-shrink gs)
          (not (empty? falling-cells)) (game-state-fall gs)
          :otherwise (ensure-gs-solvable gs))))

(defn drop-app []
  (let [game-state (atom (ensure-gs-solvable +initial-game-state+))
        canvas (proxy [JPanel] []
                 (paintComponent [g]
                   (proxy-super paintComponent g)
                   (draw-game-state g @game-state)))
        timer (Timer. 15 nil)
        timer-ticker (proxy [ActionListener] []
                       (actionPerformed [e]
                         (swap! game-state game-state-on-tick)
                         (.repaint canvas)))
        update-gs (fn [gs mouse-event]
                    (let [index (get-index-from-mouse-event mouse-event)]
                      (if (and index (game-state-ready? gs))
                        (game-state-on-click gs index)
                        gs)))
        mouse-adapter (proxy [MouseAdapter] []
                        (mouseClicked [e]
                          (swap! game-state update-gs e)))]
    (doto timer
      (.addActionListener timer-ticker))
    (doto canvas
      (.setPreferredSize (java.awt.Dimension. +window-width+ +window-height+))
      (.addMouseListener mouse-adapter))
    (doto (JFrame. "Drop")
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setContentPane canvas)
      (.pack)
      (.setVisible true))
    (.start timer)))

(drop-app)

(shutdown-agents)