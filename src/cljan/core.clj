(ns cljan.core
  (:require [clojure.core.typed :as t]
            [cljan.state-monad :refer :all]))

;;; private interface

(defn- next-id [state]
  (let [current-counter (:entity-id-counter state)]
    [current-counter
     (assoc state
       :entity-id-counter (inc current-counter))]))

;;; public interface

(defn component [id]
  (fn [state]
    [(-> state
         :components
         id
         (#(not= % nil)))
     state]))

(defn make-cljan []
  {:components     {}
   :systems        {}
   :entities       {}
   :entity-id-counter 0})

(defmacro run-cljan [& body]
  `((state-do ~@body) (make-cljan)))

(defmacro run-cljan* [& body]
  `(first (run-cljan ~@body )))

(defn delete [ent]
  "Removes a given entity ID from the current state."
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
      [name (assoc state :components (assoc components name {:name name :maker maker}))])))

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
                          (assoc behaviors 
                            :components (set components)
                            :entity-group [])))])))

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
  "Add a system to the cljan universe.  A system is specified by a combination
  of components, and functions which describe operations on entities at various
  times.

  Systems are the basis for behaviors run by the game.

  Each component must exist."
  [name components behaviors]
  (state-do
   [:bind result (apply components? components)]
   (if result (state-do (system-raw name components behaviors))
       (throw (Throwable. "System refered to components that don't exist.")))))

(defn entity
  "Creates an empty entity. An entity is a bag of components. The point of
  having one is to represent something in the game with certain features that
  imply behaviors."
  []
  (state-do
   [:bind id next-id]
   [:bind entities (state-get :entities)]
   (state-assoc :entities (assoc entities id {:component-ids #{} :components {}}))
   (state-return id)))

(defn make-component [id args]
  (fn [state]
    (apply (-> state :components id :maker)
     args)))

(defn get-ent [id]
  (fn [state]
    [(-> state :entities id) state]))

(defn set-ent [id new-val]
  (fn [state]
    [id (let [entities (-> state :entities)
              new-entities (assoc entities id new-val)]
          (assoc state :entities new-entities))]))



(defn update-systems-with-ent [ent]
  (state-do 
   []))

(defn add-component-raw [entid component-id & args]
  (state-if (component-id? component-id)
   (state-do 
    [:bind component (make-component component-id args)]
    [:bind ent (get-ent entid)]
    [:let 
     components (:components ent)
     component-ids (:component-ids ent)]
    (set-ent entid (assoc ent 
                     :component-ids (conj component-ids component-id)
                     :components (assoc components component-id component))))
   (throw (Throwable. (format "Could not add component ~a to entity." component-id)))))
