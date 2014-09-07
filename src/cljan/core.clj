(ns cljan.core
  (:require [clojure.core.typed :as t]
            [cljan.state-monad :refer :all]
            [clojure.set :refer [subset?]]))

;;; private interface

(defn- next-id 
  "Returns the next ID to use for tagging entities.  Since each entity
  created by the system has a unique identifier, this is useful for
  creating them."
  [state]
  (let [current-counter (:entity-id-counter state)]
    [current-counter
     (assoc state
       :entity-id-counter (inc current-counter))]))

;;; public interface

(defn make-cljan 
  "Create an empty cljan-universe." []
  {:components     {}
   :systems        {}
   :entities       {}
   :entity-id-counter 0})

(defmacro run-cljan 
  "Given a cljan function, which is a function in the state monad which operates on a cljan state, execute that function and return two element list of [monadic-retval, final-state]. Cljan programs are large functions in the state monad, and this form executes such a function."
  [& body]
  `((state-do ~@body) (make-cljan)))

(defmacro run-cljan* [& body]
  "Identical to run-cljan, except it discards the final state and returns only the monadic return value."
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
  "Returns TRUE when ID is a component id in the cljan universe STATE."
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
  "Allows testing for multiple components existence."
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

(defn apply-entity-to-system [a-fn entity-id system-id]
  (state-do
   [:bind system (state-get :systems system-id)]
   (state-assoc-in [:systems system-id]
                   (assoc system
                     :entity-group (-> system
                                       :entity-group
                                       (a-fn entity-id))))))

(defn add-entity-to-system [entity-id system-id]
  (apply-entity-to-system conj entity-id system-id))

(defn remove-entity-from-system [entity-id system-id]
  (apply-entity-to-system disj entity-id system-id))

(defn get-ent [ent-id]
  (state-do
   [:bind entities (state-get :entities)]
   (state-return (entities ent-id))))

(defn handle-systems-for-entity [ent-id]
  "Given an entity ID, this function iterates through the systems and
  adds the entity to systems which match the components of the entity."
  (state-do 

   [:bind ent (get-ent ent-id)]
   [:let ent-component-ids (:component-ids ent)]
   [:bind systems (state-get :systems)]
   (state-map
    (fn [system-key]
      (if (subset? (-> systems system-key :components) ent-component-ids)
        (add-entity-to-system ent-id system-key)
        (state-return nil))) (keys systems))))

(defn add-component-raw [entid component-id & args]
  "Given an ENTID, a COMPONENT-ID and ARGS to be passed to the
  component constructor, this function adds that component to the
  entity.  

Adding components to entities is fundamental the functionality of
cljan, since it is how all entities define their behavior."
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
  "This function adds a component instance to an entity and assures
  that the entity joins the appropriate systems."
  (state-do
   (apply add-component-raw entid component-id args)
   (handle-systems-for-entity entid)))

(defn- remove-component-from-entity
  "Auxiliary function that makes remove-component easier on the eyes. Provides a
  new ENTITY without the COMPONENT-ID removed from the correct places."
  [entity component-id]
  (assoc entity
    :component-ids (-> entity
                       :component-ids
                       (disj component-id))
    :components    (-> entity
                       :components
                       (dissoc component-id))))

(defn remove-component
  "Removes a component with a given COMPONENT-ID from an entity with a
  given ENTITY-ID. This is useful in order to modify entities. For
  example, making an entity mortal by adding health."
  [entity-id component-id]
  (state-do
   [:bind entity (get-ent entity-id)]
   (set-ent entity-id
            (remove-component-from-entity entity component-id))
   [:bind new-entity (get-ent entity-id)]
   [:let  new-component-ids (:component-ids new-entity)]
   [:bind systems (state-get :systems)]
   (state-map
    (fn [system-key]
      (if (-> systems
              system-key
              :entity-group
              (contains? ,, entity-id))
        (if (subset? (-> systems system-key :components)
                     new-component-ids)
          (state-return nil) ;; entity should still be in this system.
          (remove-entity-from-system entity-id system-key))
        (state-return nil)))
    (keys systems))))


