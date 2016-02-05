
(ns l_system_lab.core
  (:require [quil.core :refer :all]
            [l_system_lab.student :refer :all])  )

;;
;; # Angle Utilities
;; The built-in functions use radians.  Good for computers, bad for humans.
;; These utilties will use angles.

(defonce ang-to-rad (/ (* Math/PI 2.0) 360.0))

(defn ang-cos "Take the cosine of the argument in degrees."
  [deg]
  (cos (* deg ang-to-rad)))

(defn ang-sin "Take the sine of the argument in degrees."
  [deg]
  (sin (* deg ang-to-rad)))


;; ## Turtle Commands
;;
;; The Quil system has some specific commands for drawing graphics.
;; This function runs those commands.


(defn run-turtle
  "Draws a line, if the next command is a line vector."
  [v]
  (case (first v)
    :line (let [[_ x1 y1 x2 y2] v]
            (line x1 y1 x2 y2))))

(defn l-to-turtle [init-cmds init-state init-stack]
  (loop [cmds init-cmds
         state init-state
         stack init-stack
         out []]
    (if (not-empty cmds)
      (let [c (first cmds)]
        (case c
          :<  (recur (rest cmds)
                state
                (cons state stack)
                out)
          :f  (let [nux (+ (:x state)  (ang-cos (:angle state)))
                    nuy (+ (:y state)  (ang-sin (:angle state)))]
                 (recur (rest cmds)
                   (assoc state :x nux :y nuy)
                   stack
                   (conj out [:line (:x state) (:y state) nux nuy])))
          :> (recur (rest cmds)
               (first stack)
               (rest stack)
               out)
          :+ (recur (rest cmds)
               (update-in state [:angle] + (:theta state)) stack out)
          :- (recur (rest cmds)
               (update-in state [:angle] - (:theta state)) stack out)
          (recur (rest cmds)
               state
               stack
               out)))
      out)))

;; Student function: scale-turtle
;;
;; Input: a list of [:line a b c d] vectors.  Some of the vectors may be empty: []
;; Output: a list that has been centered about coordinates 250,250, scaled to size 480x480.
;; I.e. '([:line 30 0 50 -50] [])  would be converted to '([10 250 480 10])
;;



(def dragon-curve
  {:init [:f :x]
   :rules {:x [:x :+ :y :f :+]
           :y [:- :f :x :- :y]}
   :theta 90
   :init-angle 0})

(def koch-curve
  {:init  [:f :- :- :f :- :- :f]
   :rules {:f [:f :+ :f :- :- :f :+ :f]}
   :theta 60
   :init-angle 0})

(def menger-curve
  {:init [:f :- :f :- :f :- :f]
   :rules {:f [:f :+ :f :- :f :- :f :+ :f]}
   :theta 90
   :init-angle 0})

;; # Quil graphics routines.  You don't need to do much with these.

(defn setup []
  (smooth)
  (background 200))

(def current (atom {}))

(defn l-system [pattern]
  (fn []
    (background 255)
    (stroke 255 0 0)
    (stroke-weight 1)
    (let [state {:x 100 :y 300 :theta (:theta pattern) :angle (:init-angle pattern)}
          cmds (nth (iterate #(transform % (:rules pattern)) (:init pattern)) (dec (frame-count) ))
          t-cmds (l-to-turtle cmds state nil)
          scaled-t-cmds (scale-turtle (remove-empties t-cmds))
          _ (reset! current scaled-t-cmds)]
      (doall (doseq [c scaled-t-cmds]
               (run-turtle c))))
    (no-loop)))

(defsketch l-system-sketch
  :title "An L-System"
  :setup setup
  :draw (l-system menger-curve)
  :mouse-pressed redraw
  :size [500 500])
