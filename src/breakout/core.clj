(ns breakout.core
  (:require [quil.core :as q]
            [quil.core :refer :all]
            [quil.middleware :as m]
            [quil.core :as q :include-macros true]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ;(q/background 200)
  ;  {:color 0}
  ; {:angle 0})
  ;(smooth)
  ;{:x 125 :y 450 :x1 175 :y1 450}
  (background 230 230 230)
  (stroke 0, 0 0)
  (stroke-weight 10))

(defn update-state [state] )

(defn draw-state [state]
  (q/background 240)
 (let [x (mouse-x)                           ;; line
       y (mouse-y)]
    (if(and (> x 0) (> y 0) (> x (width)) (> y (height)))
      (conj state 0)
  (line [(- x 25) 450] [(+ x 25) 450])))
 (doseq [x [5 25 45 65 85 105 125 145 165 185 205 225 245 265 285]
         y [40 70 100 130 160]]
 (let [w 10
       h 20
       r 1]
  (rect x y w h r)))
  (q/point 150 300))


(q/defsketch breakout
  :title "Break me out!"
  :size [300 500]
  :setup setup
  :draw draw-state
  :update update-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
