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
                            :first-entity-id nil
                            :last-entity-id nil)))])))

(defn first-element-of-system 
  "Given a system id, return the first entity in that system (nil if empty)"
  [system-id]
  (state-get :systems system-id :first-entity-id))

(defn last-element-of-system [system-id]
  "Given a system id, return the last entity in that system (nil if empty)"
  (state-get :systems system-id :last-entity-id))


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
   (state-assoc :entities (assoc entities id {:id id :component-ids #{} :components {} :nexts {} :prevs {}}))
   (state-return id)))

(defn next-entity 
  "Low level function to iterate forward through the entities through
  a system."
  [entity-id system-id]
  (state-get :entities :nexts :system-id))

(defn set-next-entity 
  "Given an entity and a system id, set the next entity in the entity-linked-list.
   This is a low level function, see add-entity-to-system and
  remove-entity-from-system."
  [of-entity-id system-id to-entity-id]
  (print "set-next-entity" of-entity-id system-id to-entity-id)
  (state-assoc-in [:entities of-entity-id :nexts system-id] to-entity-id))

(defn set-prev-entity 
  "Given an entity and a system id, set the next entity in the entity-linked-list.
  This is a low level function, see add-entity-to-system and
  remove-entity-from-system."
  [of-entity-id system-id to-entity-id]
  (print "set-prev-entity" of-entity-id system-id to-entity-id)
  (state-assoc-in [:entities of-entity-id :prevs system-id] to-entity-id))


(defn previous-entity 
  "Given and entity and a system ID this gives the previous entity in
  the entitiy list for that system."
  [entity-id system-id]
  (state-get :entities :prevs :system-id))

(defn get-ent [id]
  "Given an entity ID, fetches the actual entity.  The player probably
  should never see the actual entity data, but this function is useful
  for internal use and special circumstances."
  (fn [state]
    [(-> state :entities id) state]))

(defn add-entity-to-system 
  "Adds the entity to the given system.
  This is a low-level function, and it doesn't handle making sure that
  the components of the system are added to the entity."
  [entity-id system-id]  
  (state-do 
   [:bind last-entity-in-system (last-element-of-system system-id)]
   (if (nil? last-entity-in-system)
     (state-do 
      (state-assoc-in [:systems system-id :first-entity-id] entity-id)
      (state-assoc-in [:systems system-id :last-entity-id] entity-id))
     (state-do
      (set-next-entity last-entity-in-system system-id entity-id)
      (set-prev-entity entity-id system-id last-entity-in-system)
      (set-next-entity entity-id system-id nil)
      (state-assoc-in [:systems system-id :last-entity-id] entity-id)))))

(defn entity-belongs-in-system? 
  "Returns true (into the state monad) when the entity belongs in the system."
  [entity-id system-id]
  (state-do 
   [:bind 
    entity-components (state-get :entities entity-id :component-ids)
    system-components (state-get :systems system-id :components)]
   (state-return system-components entity-components)))

(defn remove-entity-from-system 
  "Removes the entity from the given system.
   This is a low-level function and it doesn't handle removing the
  components of the system from the entity."
  [entity-id system-id]
  (print "removing entity from system: " entity-id system-id)
  (state-do 
   [:bind 
    head (first-element-of-system system-id)
    tail (last-element-of-system system-id)
    prev (previous-entity entity-id system-id)
    next (next-entity entity-id system-id)]
   [:aside (print {:head head :tail tail :prev prev :next next})]
   (cond 
    (and (nil? prev) (nil? next)) 
    (state-do 
     (state-assoc-in [:systems system-id :first-entity-id] nil)
     (state-assoc-in [:systems system-id :last-entity-id] nil))
    (and (nil? prev) (not (nil? next)))
    ;; entity is the head of the system linked list
    (state-do 
     (state-assoc-in [:systems system-id :first-entity-id] next)
     (set-prev-entity next system-id nil))
    (and (not (nil? prev)) (nil? next))
    ;; entity is the tail of the system linked list
    (state-do 
     (state-assoc-in [:systems system-id :last-entity-id] prev)
     (set-next-entity prev system-id nil))
    :otherwise 
    ;; somewhere in the middle 
    (state-do 
     (set-next-entity prev system-id next)
     (set-prev-entity next system-id prev)))))

(defn make-component [id & args]
  "Produce an instance of a component with ID, by passing args"
  (fn [state]
    [(apply
      (-> state :components id :maker)
      args)
     state]))

(defn set-ent [id new-val]
  "Update an entity at ID with a new-value NEW-VAL.  Again, since the
  entity must have certain information associated with it, the use of
  the library probably will not use this method directly."
  (fn [state]
    [id (let [entities (-> state :entities)
              new-entities (assoc entities id new-val)]
          (assoc state :entities new-entities))]))

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
        (do 
          
          (add-entity-to-system ent-id system-key))
        (do 
          (state-return nil)))) 
    (keys systems))))

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

(defn system-for-each 
  "Call the function STATE-FUN on each member of the system SYSTEM-ID,
  for side effects in the state monad."
  [system-id state-fun]
  (state-do 
   [:bind 
    head (first-element-of-system system-id)    
    state extract-state]
   [:aside (print head)]
   (loop [current head
          state state]
     (if (nil? current) (set-state state)
         (let [[next new-state] 
               ((state-do 
                 (state-fun current)
                 (next-entity current system-id)) 
                state)]
           (recur next new-state))))))

(defn system-reduce 
  "Call the reduction function STATE-FUN-REDUCER on each member of the
  system SYSTEM-ID and the previous value of the reduction function,
  finally, return into the state monad the result of the reduction."
  [system-id state-reducer init]
  (state-do 
   [:bind 
    head (first-element-of-system system-id)
    state extract-state]
   [:aside (print :head head)]
   (loop [current head
          acc init
          state state]
     (if (nil? current) (state-do (set-state state)
                                  (state-return acc))
         (let [[[new-acc next-ent] new-state] 
               ((state-do 
                 [:bind 
                  new-acc (state-reducer acc current)
                  next-ent (next-entity current system-id)]
                 (state-return [new-acc next-ent])) 
                state)]
           (recur next-ent new-acc new-state))))))

(defn system-entities-snapshot 
  "Return the list of entities (as entity ids) "
  [system-id]
  (system-reduce system-id
                 (fn [acc ent]
                   (state-return (conj acc ent))) []))

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
   [:aside (print 'new-entity new-entity)]
   (state-map
    (fn [system-key]
      (if (subset? (-> systems system-key :components)
                   new-component-ids)
        (state-return nil) ;; entity should still be in this system.
        (remove-entity-from-system entity-id system-key)))
    (keys systems))))
