(ns cljan.exmaple
  (:require [cljan.core :refer :all]
            [cljan.state-monad :refer :all]))

(defn init-components []
  (state-do
   (component :growth-stage identity)
   (component :growth-rate identity)))

(defn grow [growth-stage growth-rate ent]
  (set-ent-component ent :growth-stage (+ growth-rate growth-stage)))

(defn init-systems []
  (state-do
   (system :growing-things [:growth-stage :growth-rate]
     {:every grow})))

(defn init-entities []
  (state-repeat 10
                (fn [i]
                  (state-do
                   [:bind e (entity)]
                   (add-component e :growth-stage 0)
                   (add-component e :growth-rate (rand-int 10))))))


(defn init []
  (state-do
   (init-components)
   (init-systems)
   (init-entities)))

(defn update []
  (execute-system :growing-things))


(def world (atom (run-cljan-for-state (init))))

(defn update-world! []
  (swap! world (fn [world]
                 (update-cljan world (update)))))

;; (loop [i 0]
;;   (if (< i 10) (do (update-world!) (recur (+ i 1))) @world))

;; (map (fn [key]
;;        (-> ((:entities @world) key) :components :growth-stage))
;;      (keys (:entities @world)))


;; (map (fn [key]
;;        (-> ((:entities @world) key) :components :growth-stage))
;;      (keys (:entities @world)))

;; (0 140 140 120 40 180 20 120 40 160)
