(ns l_system_lab.t-student
  (:use midje.sweet)
  (:use [l_system_lab.student]))

(facts "about numners"
       (fact "Floating point is close enough sometimes."
             10.0 => (roughly 10.001)
             9.0 => (roughly 9.00001)))
