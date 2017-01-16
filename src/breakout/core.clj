(ns breakout.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; define width and height for bricks
(def brick-width 19)
(def brick-height 29)

(defn draw-line
  "Make a line to follow a mouse"
  [state]
  (q/stroke-weight 10)
  (let [x (q/mouse-x)
        y (q/mouse-y)]
    (if(and (> x 0) (> y 0) (> x (q/width)) (> y (q/height)))
    (conj state 0)
    (q/line [(- x 25) 450] [(+ x 25) 450]))))

(def brix
  "Upper left coordinates of each brick"
  (let [x-val (into [] (for [x (range 0 15)
      :let [y (+ (* x 20) 1)]] y))
        y-val  [40 70 100 130 160]
        z-val []]
   (doall (for [x x-val
                y y-val]
            (into [] (conj z-val x y))))))

;; an atom to temporary store data
(def new (atom ()))

;; an atom to store all outer pixels of all bricks
(def grid (atom ()))

(defn generate
  "Generate data structure for all outer pixels of one brick"
  [[a b]]
  (let [x (take brick-width (iterate inc a))
        x1 (take brick-width (repeat b))
        y (take brick-height (repeat a))
        y1 (take brick-height (iterate inc b))
        xx (take brick-width (repeat (+ b brick-height)))
        yy (take brick-height (repeat (+ a brick-width)))]
    (swap! new conj (interleave x x1))      ;; top
    (swap! new conj (interleave x xx))      ;; bottom
    (swap! new conj (interleave y y1))      ;; left
    (swap! new conj (interleave yy y1))     ;; right
    (reset! new (partition 2 (flatten @new)))
    (swap! new conj [a b])
    (swap! grid conj @new))
  (reset! new ()))

;; define initial colours for bricks
(def colour1 (atom '(255 13 169)))
(def colour2 (atom '(13 255 169)))
(def colour3 (atom '(169 13 255)))
(def colour4 (atom '(255 169 13)))
(def colour5 (atom '(13 169 255)))

(defn draw-bricks
  "Draw a grid of bricks on the screen"
  []
  (q/stroke-weight 0)
  (let [upper-left (map #(first %) @grid)]
  (doseq [[x y] upper-left]
    (if (= y 40)
      (q/fill (first @colour1) (second @colour1) (last @colour1)))
    (if (= y 70)
      (q/fill (first @colour2) (second @colour2) (last @colour2)))
    (if (= y 100)
      (q/fill (first @colour3) (second @colour3) (last @colour3)))
    (if (= y 130)
      (q/fill (first @colour4) (second @colour4) (last @colour4)))
    (if (= y 160)
      (q/fill (first @colour5) (second @colour5) (last @colour5)))
    (let [w brick-width
          h brick-height
          r 5]
      (q/rect x y w h r)))))

;; make a ball
(def ball (atom {}))

;; make a ball directions
(def ball-dir (atom []))

(defn next-ball
  "Calculate the new position for the ball after moving one step into the direction"
  [ball dir]
  (let [dx (first dir)
        dy (second dir)]
    (assoc ball :x (+ (:x ball) dx)
                :y (+ (:y ball) dy))))

(defn draw-ball
  "Draw a ball"
  [r]
  (q/stroke-weight 1)
  (q/fill 0)
  (q/ellipse (:x r) (:y r) (:w r) (:h r)))

(defn repaint
  "Change colours for bricks"
  []
  (let [a [(rand-int 255) (rand-int 255) (rand-int 255)]
        b [(rand-int 255) (rand-int 255) (rand-int 255)]
        c [(rand-int 255) (rand-int 255) (rand-int 255)]
        d [(rand-int 255) (rand-int 255) (rand-int 255)]
        e [(rand-int 255) (rand-int 255) (rand-int 255)] ]
    (reset! colour1 (into '() a))
    (reset! colour2 (into '() b))
    (reset! colour3 (into '() c))
    (reset! colour4 (into '() d))
    (reset! colour5 (into '() e))))

(defn collision
  "Check if ball collided with a brick, change directions for a ball and repaint the bricks"
  [coordinates]
  (let [x (.indexOf (map #(.contains % coordinates) @grid ) true) ]
  (if (> x -1)
    (do
    (swap! ball-dir (fn [[a b]] [a (- b)]))
    (repaint)
    (reset! grid (apply merge (drop (+ x 1) @grid) (take x @grid)))
    ;(reset! grid '())
      ))))

(defn win
  "Check if grid is empty and display win screen"
  []
  (if (empty? @grid)
    (do
      (q/fill (rand-int 255) (rand-int 255) (rand-int 255))
      (q/text-size 50)
      (q/text "You won!!!" 25 150 )
      (reset! ball-dir [0 0])
      (play-again))))

(defn lose
  "Check if ball's position is bellow the paddle and display lose screen"
  []
  (if (> (:y @ball) 446)
    (do
      (q/fill (rand-int 255) (rand-int 255) (rand-int 255))
      (q/text-size 50)
      (q/text "You lost..." 35 300 )
      (play-again))))

(defn play-again
  "After winning or losing, ask to play again"
  []
  (q/fill 0)
  (q/text-size 30)
  (q/text "Click to play again!" 10 400 )
  (if (q/mouse-pressed?)
    (setup)))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 60)
  (q/background 230)
  (q/stroke-weight 1)
  (reset! ball {:x 150 :y 300 :w 15 :h 15})
  (reset! ball-dir [-1 1])
  ;; generate outer pixels of input bricks
  (println (#(map generate %) brix)))   ;; a hack...?

;; put it all together
(defn draw-state [state]
  (q/background 240)
  (draw-line state)
  (draw-bricks)
  (draw-ball @ball)
  (win)
  (lose))

;; update
(defn update-state [state]
  ;; move a ball to a next position
  (collision (take 2 (vals @ball)))
  (swap! ball next-ball @ball-dir)
  ;; invert x direction
  (when (or (> (:x @ball) 300) (< (:x @ball) 0))
    (swap! ball-dir (fn [[x y]] [y (- x)])))
  ;; invert y direction & make a ball bounce off the paddle
  (when (or (and (= (:y @ball) 445) (and (>= (:x @ball) (- (q/mouse-x) 25)) (<= (:x @ball) (+ (q/mouse-x) 25))))
            (< (:y @ball) 0))
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
