(ns cljan.core
  (:require [clojure.core.typed :as t]
            [cljan.state-monad :refer :all]))

(defn make-cljan [] 
  {:components {}
   :systems {}
   :entities []
   })

(defmacro run-cljan [& body]
  `((state-do ~@body) (make-cljan)))

(defmacro run-cljan* [& body]
  `(first (run-cljan ~@body )))

(defn delete [ent]
  (fn [state]
    (assoc state :entities 
           (filter #(not= % ent) (:entities state)))))

(defn component 
  "Adds a component to the cljan universe.
A component is a piece of data which allows us to construct
  behaviors by building systems, which operate on entities.."
  [name maker]
  (fn [state]
    (let [components (:components state)]
      [name (assoc state :components (assoc components name {:name name}))])))

(defn component? [id]
  (fn [state]
    [(-> state 
         :components 
         (get ,, id)
         (#(not= % nil)))
     state]))

(defn system-raw
  "Add a system to the cljan universe.  A system is specified by a
  combination of components, and functions which describe operations
  on entities at various times.
 
Systems are the basis for behaviors run by the game."
  [name components behaviors]
  (fn [state]
    (let [systems (:systems state)]
      [name (assoc state :systems 
                   (assoc systems name 
                          (assoc behaviors :entity-group [])))])))

(defn components? 
  ([] 
     (state-return true))
  ([first & rest]
     (state-do 
      [:bind c? (component? first)]
      (if c? 
        (apply components? rest)
        (state-return false)))))

(defn system 
  "Add a system to the cljan universe.  A system is specified by a
  combination of components, and functions which describe operations
  on entities at various times.
 
Systems are the basis for behaviors run by the game. 

Each component must exist."
  [name components behaviors]
  (state-do 
   [:bind result (apply components? components)]
   (if result (state-do (system-raw name components behaviors))
        (throw (Throwable. "System refered to components that don't exist.")))))


