(ns breakout.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def brick-width 19)
(def brick-height 29)

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

;; TODO - improve this
(defn collision
  "check if ball collided with a brick...."
  [coordinates]
  (let [x (.indexOf (map #(.contains % coordinates) @grid ) true) ]
  (if (> x -1)
    ((println "yay!")
    (println coordinates)
    (reset! grid (apply merge (drop (+ x 1) @grid) (take x @grid)))))))

;; draw a grid of bricks on the screen
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
  (println (#(map generate %) brix)))   ;; a hack...?

;; put it all together
(defn draw-state [state]
  (q/background 240)
  (draw-line state)
  (draw-bricks)
  (draw-ball @ball))

;; update
(defn update-state [state]
  ;; move a ball to a next position
  (collision (take 2 (vals @ball)))
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
