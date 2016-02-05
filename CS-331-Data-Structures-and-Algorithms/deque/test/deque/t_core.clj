(ns deque.t-core
  (:use midje.sweet)
  (:use [deque.core])
  (:import [deque.core Deque] ))

(facts "about this lab"
       (fact "the student never started it"
             (+ 1 2) => 3))

(facts "checking size of Deque."
       (fact "get proper size"
             (:size (Deque. '(1 2 3) '() 3)) => 3)
       (fact "checking size after push front"
             (:size (push-front (Deque. '(1 2 3) '(4 5 6) 6) 7))=> 7)
       (fact "Checking size after pop-back and make sure it doesn't go negative"
             (:size (pop-back (Deque. '(1 2 3) '() 3))) => 2
             (:size (pop-back (Deque. '() '() 0))) => 0)
       (fact "Checking size after pop-front"
             (:size (pop-front (Deque.  '(1 3 6) '(2 3) 5))) => 4
             (:size (pop-front (Deque. '() '(2 3) 2)))=> 1))

(facts "checking lists of Deque."
       (fact "Checking lists and size after push-back"
             (push-back (Deque. '(1 2 3) '(4 5) 5) 8) => (Deque. '(1 2 3) '(8 4 5) 6))
       (fact "Checking the back list after flip-front"
             (:back (flip-front (Deque. '() '(2 3) 2))) => '()
             (flip-front (Deque. '(1 4 6) '() 3)) => (Deque. '(1 4 6) '() 3))
       (fact "Checking the front list after flip-back"
             (:front (flip-back (Deque. '() '(5 6 7) 3))) => '()
             (flip-back (Deque. '(1 2) '() 2)) => (Deque. '() '(2 1) 2))
       (fact "Checking back doesn't flip the list"
             (back (Deque. '(1 2 3) '(2 4) 5)) => 2
             (back (Deque. '(1 2 3) '() 3)) => 3)
       (fact "Front references back list"
             (front (Deque. '(1 2 3) '(2 4) 5)) => 1
             (front (Deque. '() '(1 2 3) 3)) => 3)
       (fact "Checking if flip front flips always"
             (flip-front (Deque. '(1 2 3) '() 3)) => (Deque. '(1 2 3) '() 3)
             (flip-front (Deque. '() '() 0)) => (Deque. '() '() 0)))
