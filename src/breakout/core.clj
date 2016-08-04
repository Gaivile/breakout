(ns breakout.core
  (:require [quil.core :as q]
            [quil.core :refer :all]
            [quil.middleware :as m]
            [quil.core :as q :include-macros true]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  {:color1 0
    :angle 0}
  (background 230)
  (stroke-weight 1))

;; make a line to follow a mouse
(defn draw-line []
  (stroke-weight 10)
  (let [x (mouse-x)
        y (mouse-y)]
    (if(and (> x 0) (> y 0) (> x (width)) (> y (height)))
    (conj state 0)
    (line [(- x 25) 450] [(+ x 25) 450]))))

;; make a list of x-values
(defn grid-x [i]
  (for [x (range 0 15)
      :let [y (+ (* x 20) 1)]]
      y))

;; make vectors out of x and y values
(defn x-val []
  (into [] (grid-x 1)))
(defn y-val []
  [40 70 100 130 160])


;; TODO
(defn create-color []       ;; create random color for each row
  )                         ;; how do i make it nice??

(def color5 (atom {:r5 155 :g5 155 :b5 155}))

;; draw a grid of bricks on the screen
(defn draw-bricks []
  (stroke-weight 0)
  (doseq [x (x-val)       ;; 15 nums  -make the whole structure atomic, change one element at a time
         y (y-val)]       ;; 5         -change colour when touched
  (if (= y 40)
    (q/fill 255 255 255))
  (if (= y 70)
    (q/fill 0 0 0))
  (if (= y 100)
    (q/fill 255 0 0))
  (if (= y 130)
    (q/fill 0 255 255))
  (if (= y 160)
    (q/fill (:r5 @color5) (:g5 @color5) (:b5 @color5)))
 (let [w 18
       h 28
       r 5]
  (q/rect x y w h r))))

 (def line5 (atom {:x5 1 :y5 160 :w 300 :h 28 :r 5}))

 (defn draw-line5 [l5]
  (q/stroke-weight 0)
  (q/fill (:r5 @color5) (:g5 @color5) (:b5 @color5))
 (q/rect (:x l5) (:y l5) (:w l5) (:h l5) (:r l5)))

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

;; put it all together
(defn draw-state [state]
  (q/background 240)
  (draw-line)
  (draw-bricks)
  (draw-ball @ball))

;; update
(defn update-state [state]
  ;; move a ball to a next position
  (swap! ball next-ball @ball-dir)
  ;; invert x direction
  (when (or (> (:x @ball) 300) (< (:x @ball) 0))
    (swap! ball-dir (fn [[x y]] [y (- x)])))
  ;; invert y direction & make a ball bounce off the paddle
  (when (or (and (> (:y @ball) 445) (and (>= (:x @ball) (- (mouse-x) 25)) (<= (:x @ball) (+ (mouse-x) 25))))
            (< (:y @ball) 0)
            ;; make a ball bounce off the bricks
            (and (<= (:y @ball) 195) (and (= (:r5 @color5) 155) (= (:g5 @color5) 155) (= (:b5 @color5) 155))))
    (swap! ball-dir (fn [[x y]] [x (- y)])))
  )

;; run
(q/defsketch breakout
  :title "Break me out!"
  :size [301 500]
  :setup setup
  :draw draw-state
  :update update-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
