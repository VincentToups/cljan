(ns cljan.core-test
  (:require [cljan.core :refer :all]
            [cljan.state-monad :refer :all]))

(defn init-components 
  []
  (state-do 
   (component :counter (fn [init] init))))

(defn inc-counter [counter entity]
  (print "Inside inc-counter counter entity" counter entity "\n")
  (set-ent-component entity :counter (+ 1
                                        counter)))

(defn init-systems []
  (state-do 
   (system :counters [:counter] {:every inc-counter})))

(defn init-entities []
  (state-do 
   [:bind 
    e1 (entity)
    e2 (entity)]
   (add-component e1 :counter 0)
   (add-component e2 :counter 10)))

(defn get-counter-value [ent-id]
  (get-ent-component ent-id :counter))

(defn go []
  (run-cljan 
   (init-components)
   (init-systems)
   (init-entities)
   (execute-system :counters)
   (execute-system :counters)
   (system-map :counters get-counter-value)
   ;(system-entities-snapshot :counters)
   ))

(go)
