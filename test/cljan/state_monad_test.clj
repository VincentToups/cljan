(ns cljan.state-monad-test
  (:require [clojure.test :refer :all]
            [cljan.state-monad :refer :all]))

(deftest assoc-test
  (testing "State-monad with assoc support."
    (is (= [10 {:x 10 :y 11}]
           ((state-do
             (state-assoc :x 10)
             [:bind state extract-state]
             [:aside (println state)]
             (state-assoc :y 11)
             [:bind state extract-state]
             [:aside (println state hello)]
             (state-get :x)) (hash-map))))))



(run-tests)




