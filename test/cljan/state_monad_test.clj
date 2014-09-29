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
             [:aside (println state)]
             (state-get :x)) (hash-map))))))

(deftest multibind-test
  (testing "Whether one can put multiple monadic bindings into a single bind form."
    (is (= [21 {:x 10 :y 11}]
           ((state-do
             [:bind 
              x (state-get :x)
              y (state-get :y)]
             (state-return (+ x y))) 
            {:x 10 :y 11})))))

(deftest map-test
  (testing "Test the functionality of map in the state monad."
    (is (= [[2 3 4] {:last-x 3}]
           ((state-map (fn [x]
                         (state-do
                          (state-assoc :last-x x)
                          (state-return (+ x 1))))
                       [1 2 3]) {})))))

(run-tests)



