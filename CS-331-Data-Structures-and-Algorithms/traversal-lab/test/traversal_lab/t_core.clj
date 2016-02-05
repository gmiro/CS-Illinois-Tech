(ns traversal_lab.t-core
  (:use midje.sweet)
  (:use [traversal_lab.core]))

(facts "about the student"
  (fact "they never got started."
        (+ 1 2)  => 123))
