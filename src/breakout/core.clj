(ns breakout.core
  (:require [quil.core :as q]
            [quil.core :refer :all]
            [quil.middleware :as m]
            [quil.core :as q :include-macros true]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ;(q/background 200)
    {:color 0
     :angle 0}
  ;(smooth)
  (background 230 230 230)
 ; (stroke 0, 0 0)
  (stroke-weight 1))


(defn draw-line []                                   ;; line
  (stroke-weight 10)
  (let [x (mouse-x)
       y (mouse-y)]
    (if(and (> x 0) (> y 0) (> x (width)) (> y (height)))
      (conj state 0)
  (line [(- x 25) 450] [(+ x 25) 450]))))

(defn grid-x [i]
  (for [x (range 0 15)
      :let [y (+ (* x 20) 1)]]
      y))

(defn x-val []
  (into [] (grid-x 1)))

(defn y-val []
  [40 70 100 130 160])

(defn create-color []       ;; create random color for each row
  )                         ;; how do i make it look nice??

(defn draw-bricks []
  (stroke-weight 0)
  (doseq [x (x-val)       ;; 15 nums
         y (y-val)]       ;; 5 nums
  (if (= y 40)
    (q/fill 255 255 255))
  (if (= y 70)
    (q/fill 0 0 0))
  (if (= y 100)
    (q/fill 255 0 0))
  (if (= y 130)
    (q/fill 0 255 255))
  (if (= y 160)
    (q/fill 155 155 155))
 (let [w 18
       h 28
       r 5]
  (q/rect x y w h r))))

(def ball (atom {:x 150 :y 300 :w 15 :h 15}))

(def ball-dir (atom [-2 3]))

(defn next-ball [ball dir]
  (let [dx (first dir)
        dy (second dir)]
    (assoc ball :x (+ (:x ball) dx)
                :y (+ (:y ball) dy))))

(defn draw-ball [r]
  (q/stroke-weight 1)
  (q/fill 0)
  (q/ellipse (:x r) (:y r) (:w r) (:h r)))

(defn draw-state [state]
  (q/background 240)
  (draw-line)
  (draw-bricks)
  (draw-ball @ball))

(defn update-state [state]
  (swap! ball next-ball @ball-dir))

(q/defsketch breakout
  :title "Break me out!"
  :size [301 500]
  :setup setup
  :draw draw-state
  :update update-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
