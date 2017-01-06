(ns breakout.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; -----------------------------------------------
;; TRYING OUT A DIFFERENT DATA STRUCTURE
;; -----------------------------------------------

;(def brick-width 19)
;(def brick-height 29)

(def brick-width 2)
(def brick-height 3)

(def x-val
 (into [] (for [x (range 0 15)
      :let [y (+ (* x 20) 1)]]
      y)))

x-val

(def y-val
  [40 70 100 130 160])

y-val

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

;; an atom to temporary store data
(def new (atom ()))
new
(first @new)
(first (first @new))

;; an atom to store all outer pixels of all bricks
(def grid (atom ()))
grid

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
    (swap! new conj (partition 2 (interleave x x1)))      ;; top
    (swap! new conj (partition 2 (interleave x xx)))      ;; bottom
    (swap! new conj (partition 2 (interleave y y1)))      ;; left
    (swap! new conj (partition 2 (interleave yy y1)))     ;; right
    (swap! grid conj @new))
  (reset! new ()))

(generate [1 40])

;; generate outer pixels of input bricks
(#(map generate %) '([1 40] [20 60] [50 80]))

grid
;; => atom[((((52 80) (52 81) (52 82)) ((50 80) (50 81) (50 82)) ((50 83) (51 83)) ((50 80) (51 80)))
;;      (((22 60) (22 61) (22 62)) ((20 60) (20 61) (20 62)) ((20 63) (21 63)) ((20 60) (21 60)))
;;      (((3 40) (3 41) (3 42)) ((1 40) (1 41) (1 42)) ((1 43) (2 43)) ((1 40) (2 40))))]

;; get a map of key val of each brick
(zipmap (take (count @grid) (iterate inc 1)) @grid)
;; => {1 (((52 80) (52 81) (52 82)) ((50 80) (50 81) (50 82)) ((50 83) (51 83)) ((50 80) (51 80))),
;;   2 (((22 60) (22 61) (22 62)) ((20 60) (20 61) (20 62)) ((20 63) (21 63)) ((20 60) (21 60))),
;;   3 (((3 40) (3 41) (3 42)) ((1 40) (1 41) (1 42)) ((1 43) (2 43)) ((1 40) (2 40)))}

;; TODO - find the right key val in the whole structure, get it's key, remove from grid

;; -----------------------------------------------


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

(grid-x 1)

;; make vectors out of x and y values
(def x-val
  (into [] (grid-x 1)))

(def y-val
  [40 70 100 130 160])

x-val
y-val

(def bricks (atom #{}))

(first @bricks)
(second @bricks)
(last @bricks)

(def bbb (atom []))
bbb

(defn build-grid []
   (doall (for [x x-val
         y y-val]
     (reset! bricks [x y]))))


bricks

(build-grid)

(.contains [1 2 3] (- 15 5))

#_(defn detect-collision [some-val]
  (if (.contains y-val (:y some-val))
    )
  )

;; if :y ball = value which is in y-val (.contains), check for x-val (1-20, 21-30, 31-40, etc)
;; if :x ball = value which is in x-val, check for y-val (40-69, 70-99, 100-129, etc) - get thje first value (40, 70, etc)
;; then remove from "bricks"

x-val
y-val

(.contains x-val 21)

(.contains (range 1 18) 15)

(def example [41 40])

example

(defn fff [data]
  (if (and (>= (first data) (first example)) (<= (first data) (+ (first example ) 18)))
    "works"
    "no works"))

(fff [40 40])

(map #(str "Hello " % "!" ) ["Ford" "Arthur" "Tricia"])



(defn ggg [data]
  (if (.contains (range (first example) (+ (first example) 18)) (first data))
    "yay"
    "nay"))

(ggg [80 40])



;; draw a grid of bricks on the screen
(defn draw-bricks []
  (stroke-weight 0)
  (doseq [[x y] @bricks]
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
      (q/rect x y w h r))))

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

;; TODO - function to make a ball bounce back off the objects
(defn bounce [r b]
  (- (/ (- (:x b) (:x r))
        (:h r))
     0.5))




(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  (background 230)
  (stroke-weight 1)
  {:ball [1 2]}
  (build-grid))

(swap! bricks disj [1 40])

;; put it all together
(defn draw-state [state]
  (q/background 240)
  (draw-line)
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
  (when (or (and (> (:y @ball) 445) (and (>= (:x @ball) (- (mouse-x) 25)) (<= (:x @ball) (+ (mouse-x) 25))))
            (< (:y @ball) 0)
            ;; make a ball bounce off the bricks
            (<= (:y @ball) 190))
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
