(ns breakout.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def brick-width 19)
(def brick-height 29)

;;(.contains (partition 2(val (first (zipmap sample (sort-by first (map #(apply concat %) @grid)))))) '(1 40))

#_(defn collides?
  "Collision detection"
  [coord]
  ;; (partition 2(val (first (zipmap sample (sort-by first (map #(apply concat %) @grid))))))
  ;; let [grid keys = x
      ;;  grid values = y (partition 2 on values) ]
  ;; check if values contain coord, if so - remove the right key-val from the grid
  ;; another sample tried before - needs improvement:
  ;; (.contains (partition 2(val (first (zipmap sample (sort-by first (map #(apply concat %) @grid)))))) '(1 40))
  )

;;(map #(vector (first %) (* 2 (second %)))
         ;;   {:a 1 :b 2 :c 3})

;; TODO - finish this
#_(defn collision
  "check if ball collided with a brick...."
  [coordinates]
  (+ (first coordinates) (last coordinates)))

;; TODO - find the right key val in the whole structure, get it's key, remove from grid

;; make a line to follow a mouse
(defn draw-line [state]
  (q/stroke-weight 10)
  (let [x (q/mouse-x)
        y (q/mouse-y)]
    (if(and (> x 0) (> y 0) (> x (q/width)) (> y (q/height)))
    (conj state 0)
    (q/line [(- x 25) 450] [(+ x 25) 450]))))

;; upper left coordinates of each brick
(def brix
  (let [x-val (into [] (for [x (range 0 15)
      :let [y (+ (* x 20) 1)]] y))
        y-val  [40 70 100 130 160]
        z-val []]
   (doall (for [x x-val
                y y-val]
            (into [] (conj z-val x y))))))

brix
;; => ([1 40] [1 70] [1 100] [1 130] [1 160] [21 40] [21 70] [21 100] [21 130] [21 160]
;;   [41 40] [41 70] [41 100] [41 130] [41 160] [61 40] [61 70] [61 100] [61 130] [61 160]
;;   [81 40] [81 70] [81 100] [81 130] [81 160] [101 40] [101 70] [101 100] [101 130] [101 160]
;;   [121 40] [121 70] [121 100] [121 130] [121 160] [141 40] [141 70] [141 100] [141 130] [141 160]
;;   [161 40] [161 70] [161 100] [161 130] [161 160] [181 40] [181 70] [181 100] [181 130] [181 160]
;;   [201 40] [201 70] [201 100] [201 130] [201 160] [221 40] [221 70] [221 100] [221 130] [221 160]
;;   [241 40] [241 70] [241 100] [241 130] [241 160] [261 40] [261 70] [261 100] [261 130] [261 160]
;;   [281 40] [281 70] [281 100] [281 130] [281 160])

;; an atom to temporary store data
(def new (atom ()))

;; an atom to store all outer pixels of all bricks
(def grid (atom ()))

;; TODO - refactor so [a b] it's added to grid as a key for each list of conjoined lists
(defn generate
  "Generate data structure for all outer pixels of one brick"
  [[a b]]
  (let [;one-brick ()
        x (take brick-width (iterate inc a))
        x1 (take brick-width (repeat b))
        y (take brick-height (repeat a))
        y1 (take brick-height (iterate inc b))
        xx (take brick-width (repeat (+ b brick-height)))
        yy (take brick-height (repeat (+ a brick-width)))]
    (swap! new conj (interleave x x1))      ;; top
    (swap! new conj (interleave x xx))      ;; bottom
    (swap! new conj (interleave y y1))      ;; left
    (swap! new conj (interleave yy y1))     ;; right
    (swap! new conj [a b])
    (swap! grid conj @new))
  (reset! new ()))

;; draw a grid of bricks on the screen
;; TODO - refactor this to take the right values from the grid (keys @grid) - or smth
(defn draw-bricks []
  (q/stroke-weight 0)
  (let [upper-left (map #(first %) @grid)]
  (doseq [[x y] upper-left]
    (if (= y 40)
      (q/fill 0 0 0))
    (if (= y 70)
      (q/fill 255 255 0))
    (if (= y 100)
      (q/fill 255 0 0))
    (if (= y 130)
      (q/fill 0 255 255))
    (if (= y 160)
      (q/fill 0 0 0))
    (let [w 18
          h 28
          r 5]
      (q/rect x y w h r)))))


;; make a ball
(def ball (atom {:x 150 :y 300 :w 15 :h 15}))

;; make a ball directions (go left x=-2; go down y = 3)
(def ball-dir (atom [-2 3]))

;; calculate the new position for the ball after moving one step into the direction
(defn next-ball [ball dir]
  (let [dx (first dir)
        dy (second dir)]
    (assoc ball :x (+ (:x ball) dx)
                :y (+ (:y ball) dy))))

;; draw a ball
(defn draw-ball [r]
  (q/stroke-weight 1)
  (q/fill 0)
  (q/ellipse (:x r) (:y r) (:w r) (:h r)))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  (q/background 230)
  (q/stroke-weight 1)
  {:ball [1 2]}
  ;; generate outer pixels of input bricks
  ;(#(map generate %) brix)
  (println (#(map generate %) brix))
  (println @grid)) ;; a hack...?

;; put it all together
(defn draw-state [state]
  (q/background 240)
  (draw-line state)
  (draw-bricks)
  (draw-ball @ball))

ball
ball-dir

;; update
(defn update-state [state]
  ;; move a ball to a next position
  (swap! ball next-ball @ball-dir)
  ;; invert x direction
  (when (or (> (:x @ball) 300) (< (:x @ball) 0))
    (swap! ball-dir (fn [[x y]] [y (- x)])))
  ;; invert y direction & make a ball bounce off the paddle
  (when (or (and (> (:y @ball) 445) (and (>= (:x @ball) (- (q/mouse-x) 25)) (<= (:x @ball) (+ (q/mouse-x) 25))))
            (< (:y @ball) 0)
            ;; make a ball bounce off the bricks
            (<= (:y @ball) 190))
    (swap! ball-dir (fn [[x y]] [x (- y)]))))

;; run
(q/defsketch breakout
  :title "Break me out!"
  :size [301 500]
  :setup setup
  :draw draw-state
  :update update-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
