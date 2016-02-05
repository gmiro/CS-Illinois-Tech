(ns deque.core)

(defrecord Deque [front back size])

;; # Your Work

(defn make-deque
  "Create an empty deque."
  []
  (Deque. '() '() 0))

(defn deque-size
  "Return the size of a deque."
  [dq]
  (:size dq))

(defn push-front
  "Adds an element to the front of the deque."
  [dq elt]
  (let [{:keys [front back size]} dq]
    (Deque. (cons elt front) back (inc size))))

(defn push-back
  "Adds an element to the back of the deque."
  [dq elt]
  (let [{:keys [front back size]} dq]
    (Deque. front (cons elt back) (inc size))))

(defn flip-front
  "Flip the back list to the front list, if necessary."
  [dq]
  (let [{:keys [front back size]} dq]
    (cond (= size 0) dq
          (empty? back) dq
          :else (Deque. (concat front (reverse back)) '() size)))
)

(defn flip-back
  "Flip the front list to the back list, if necessary."
  [dq]
  (let [{:keys [front back size]} dq]
    (cond (= size 0) dq
          (empty? front) dq
          :else (Deque. '() (concat back (reverse front)) size)))
)

(defn front
  "Return the front element of the deque.  May cause a flip."
  [dq]
  (let [{:keys [front back size]} dq]
    (cond (= size 0) nil
          (empty? front) (first (:front (flip-front dq)))
          :else (first front))))

(defn back
  "Return the back element of the deque.  May cause a flip."
  [dq]
  (let [{:keys [front back size]} dq]
    (cond (= size 0) nil
          (empty? back) (first (:back (flip-back dq)))
          :else (first back)))
)

(defn pop-front
  "Pops/dequeues an element from the front of the deque."
  [dq]
  (let [{:keys [front back size]} dq]
    (cond (= size 0) dq
          (empty? front) (pop-front (flip-front dq))
          :else (Deque. (rest front) back (dec size))))
)

(defn pop-back
  "Pops/dequeues an element from the back of the deque."
  [dq]
  (let [{:keys [front back size]} dq]
    (cond (= size 0) dq
          (empty? back) (pop-back (flip-back dq))
          :else (Deque. front (rest back) (dec size)))))
