;; You will need to rewrite this.  This code is just for show.
(ns l_system_lab.student)

(defn remove-empties
  [v]
  (filter #(not= [] %) v))

;;
(defn get-xy-scale
  "Get the scaling factor for the coordinates."
  ([v]  {:scale (/ 480 (max (- (apply max (apply map max (map #(take-nth 2 %) (map rest v)))) (apply min (apply map min (map #(take-nth 2 %) (map rest v))))) (- (apply max (apply map max (map #(take-nth 2 %) (map rest (map rest v))))) (apply min (apply map min (map #(take-nth 2 %) (map rest (map rest v))))))))
         :min-x (apply min (apply map min (map #(take-nth 2 %) (map rest v))))
         :min-y (apply min (apply map min (map #(take-nth 2 %) (map rest (map rest v))))) })
  ([v min-x min-y max-x max-y]
     {:scale (/ 480 (max (- max-x min-x) (- max-y min-y)))
      :min-x min-x
      :min-y min-y})
)

(defn scale-turtle
  "Normalizes a list of [:line ... ] vectors."
  ([v] (scale-turtle v (get-xy-scale (remove-empties v)) []))
  ([v scale out]
     ;;v ;; For initial, just return the original.
;;(if (empty? v) out
    ;;(scale-turtle
    ;;(rest v)
    ;;scale
    ;;(conj out
    	  ;;[(+ (10 (* (:scale scale) (- ((first v) 1)(:min-x scale)))))
	   ;;(+ (10 (* (:scale scale) (- ((first v) 2)(:min-y scale)))))
	   ;;(+ (10 (* (:scale scale) (- ((first v) 3)(:min-x scale)))))
	   ;;(+ (10 (* (:scale scale) (- ((first v) 4)(:min-y scale)))))
   ))


(defn transform [init-pat rules]
  (cond (empty? init-pat) []
        (contains? rules (first init-pat))
          (-> (conj (vector (get (assoc init-pat 0 (rules (first init-pat))) 0 )) (transform (vec (rest init-pat)) rules)) flatten vec)
        :else (-> (conj (vector (get init-pat 0)) (transform (vec (rest init-pat)) rules)) flatten vec)

   )
)


;; # Some fractals to start out with.  Add some of your own!
