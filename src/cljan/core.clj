(ns cljan.core
  (:require [clojure.core.typed :as t]
            [cljan.state-monad :refer :all]
            [clojure.set :refer [subset?]]))

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
                            :entity-group #{})))])))

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

(defn make-component [id & args]
  "Produce an instance of a component with ID, by passing args"
  (fn [state]
    [(apply 
      (-> state :components id :maker)
      args)
     state]))

(defn get-ent [id]
  "Given an entity ID, fetches the actual entity.  The player probably
  should never see the actual entity data, but this function is useful
  for internal use and special circumstances."
  (fn [state]
    [(-> state :entities id) state]))

(defn set-ent [id new-val]
  "Update an entity at ID with a new-value NEW-VAL.  Again, since the
  entity must have certain information associated with it, the use of
  the library probably will not use this method directly."
  (fn [state]
    [id (let [entities (-> state :entities)
              new-entities (assoc entities id new-val)]
          (assoc state :entities new-entities))]))

(defn update-systems-with-ent [ent]
  (state-do 
   []))

(defn set-entity-group-index [system-id value]
  (state-do 
   [:bind system (state-get :systems system-id)]
   (state-assoc :systems (assoc system :index value))))

(defn incr-entity-group-index [system-id]
  (state-do 
   [:bind system (state-get :systems system-id)]
   (state-assoc :systems (assoc system :index (inc (:index system))))))

(defn decr-entity-group-index [system-id]
  (state-do 
   [:bind system (state-get :systems system-id)]
   (state-assoc :systems (assoc system :index (dec (:index system))))))

(defn system-execute-every [system-id]
  (state-do 
   [:bind entity-group (state-get :systems system-id :entity-group)]
   (state-assoc-in [:system system-id :blacklist []])
   (state-assoc :currently-updating-system system-id)
   ()))

(defn add-entity-to-system [ent-id system-id]
  (state-do 
   [:bind system (state-get :systems system-id)]
   (state-assoc-in [:systems system-id]
                   (assoc system :entity-group (conj (:entity-group system) ent-id)))))

(defn get-ent [ent-id]
  (state-do 
   [:bind entities (state-get :entities)]
   (state-return (entities ent-id))))

(defn handle-systems-for-entity [ent-id]
  (state-do 
   [:bind ent (get-ent ent-id)]
   [:let ent-component-ids (:component-ids ent)]
   [:bind systems (state-get :systems)]
   (state-map 
    (fn [system-key]
      (if (subset? (-> systems system-key :components) ent-component-ids)
        (state-do (add-entity-to-system ent-id system-key))
        (state-return nil))) (keys systems))))

(defn add-component-raw [entid component-id & args]
  (state-if 
   (component? component-id)
   (state-do 
    [:bind component (apply make-component component-id args)]
    [:bind ent (get-ent entid)]
    [:let 
     components (:components ent)
     component-ids (:component-ids ent)]
    (set-ent entid (assoc ent 
                     :component-ids (conj component-ids component-id)
                     :components (assoc components component-id component))))
   (throw (Throwable. (format "Could not add component ~a to entity." component-id)))))

(defn add-component [entid component-id & args]
  (state-do 
   (apply add-component-raw entid component-id args)
   (handle-systems-for-entity entid)))

(defn remove-component 
  "Removes a component with a given COMPONENT-ID from an entity with a
  given ENTITY-ID. This is useful in order to modify entities. For
  example, making an entity mortal by adding health."
  [entity-id component-id]
  (state-do 
   [:bind entity (get-ent entity)]
   (state-return (entities ent-id))))

(run-cljan
 (component :c1 identity)
 (component :c2 identity)
 (system :s1 [:c1 :c2]
         {
          :every identity
          })
 [:bind e1 (entity)]
 (component? :c1)
 (add-component e1 :c1 10)
 (add-component e1 :c2 34)
 extract-state)
