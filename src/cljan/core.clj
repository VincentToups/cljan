(ns cljan.core
  (:require [clojure.core.typed :as t]
            [cljan.state-monad :refer :all]
            [clojure.set :refer [subset? difference]]))

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

(defmacro run-cljan-for-state [& body]
  `(second (run-cljan ~@body)))

(defmacro update-cljan [state & body]
  `(second ((state-do ~@body) ~state)))

(defn component
  "Adds a component to the cljan universe.
  A component is a piece of data which allows us to construct
  behaviors by building systems, which operate on entities.."
  ([name]
     (component name identity))
  ([name maker]
     (fn [state]
       (let [components (:components state)]
         [name (assoc state :components (assoc components name {:name name :maker maker}))]))))

(defn tag 
  "Creates a component with the given NAME but which contains no data."
  [name]
  (component name (fn [] true)))

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
                            :components-order components
                            :first-entity-id nil
                            :last-entity-id nil)))])))

(defn first-element-of-system
  "Given a system id, return the first entity in that system (nil if empty)"
  [system-id]
  (state-get-in :systems system-id :first-entity-id))

(defn last-element-of-system [system-id]
  "Given a system id, return the last entity in that system (nil if empty)"
  (state-get-in :systems system-id :last-entity-id))


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
  ([name components behaviors]
     (state-do
      [:bind result (apply components? components)]
      (if result (state-do (system-raw name components behaviors))
          (throw (Throwable. "System refered to components that don't exist.")))))
  ([name components]
     (system name components {})))

(defn entity-raw
  "Creates an empty entity. An entity is a bag of components. The point of
  having one is to represent something in the game with certain features that
  imply behaviors."
  []
  (state-do
   [:bind id next-id]
   [:bind entities (state-get-in :entities)]
   (state-assoc :entities (assoc entities id {:id id :component-ids #{} :components {} :nexts {} :prevs {}}))
   (state-return id)))

(declare add-component)
(defn entity
  "Like entity, but takes additional component descriptors of the form
  [component-id arg1 ... argN] and adds those components to the entity.
  See also entity-raw"
  [& component-descriptors]
  (state-do
   [:bind e (entity-raw)]
   (state-map
    #(apply add-component e %)
    component-descriptors)
   (state-return e)))

(defn next-entity
  "Low level function to iterate forward through the entities through
  a system."
  [entity-id system-id]
  (state-get-in :entities entity-id :nexts system-id))

(defn set-next-entity
  "Given an entity and a system id, set the next entity in the entity-linked-list.
  This is a low level function, see add-entity-to-system and
  remove-entity-from-system."
  [of-entity-id system-id to-entity-id]
  (state-assoc-in [:entities of-entity-id :nexts system-id] to-entity-id))

(defn set-prev-entity
  "Given an entity and a system id, set the next entity in the entity-linked-list.
  This is a low level function, see add-entity-to-system and
  remove-entity-from-system."
  [of-entity-id system-id to-entity-id]
  (state-assoc-in [:entities of-entity-id :prevs system-id] to-entity-id))


(defn previous-entity
  "Given and entity and a system ID this gives the previous entity in
  the entitiy list for that system."
  [entity-id system-id]
  (state-get-in :entities entity-id :prevs system-id))

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
    entity-components (state-get-in :entities entity-id :component-ids)
    system-components (state-get-in :systems system-id :components)]
   (state-return system-components entity-components)))

(defn remove-entity-from-system
  "Removes the entity from the given system.
  This is a low-level function and it doesn't handle removing the
  components of the system from the entity."
  [entity-id system-id]
  (state-do
   [:bind
    head (first-element-of-system system-id)
    tail (last-element-of-system system-id)
    prev (previous-entity entity-id system-id)
    next (next-entity entity-id system-id)]
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

  (defn get-ent "State function which returns the entity at ENT-ID"
    [ent-id]
    (state-do
     [:bind entities (state-get-in :entities)]
     (state-return (entities ent-id))))

(defn get-ent-component-ids
  "State function which returns the component ids for the specified
  entity.  Useful for testing whether an entity belongs in a system."
  [ent-id]
  (state-do
   [:bind entities (state-get-in :entities)]
   (state-return (:component-ids (entities ent-id)))))

(defn set-ent-component
  "Given a new value, set the component _value_ for the entity and
  component id to the new-state.  Used to update the state of an
  entity's component.  See also ASSOC-COMPONENT, DIP-COMPONENT
  "
  [ent-id component-id new-state]
  (state-do
   (state-assoc-in [:entities ent-id :components component-id] new-state)))

(defn get-ent-component
  "Gets the component value for component-id from the specified entity."
  [ent-id component-id]
  (state-get-in :entities ent-id :components component-id))

(defn on-ent-component
  "Call transform on the component denoted by component-id of the
  entity ent-id and return (into the state monad) the result.

  If your intent is, instead, to operate within the state-monad use
  do-with-ent-component."
  [ent-id component-id transform]
  (state-do
   [:bind component (get-ent-component ent-id component-id)]
   (state-return (transform component))))

(defn do-with-ent-component
  "Extract the component value indicated by component-id from ent-id,
  and then call block on the result, executing the returned
  state-function on the current state.

  Use this to extract a component and then perform state actions with
  it.  If you wish instead to simply transform a component value or
  extract part of it, use `on-ent-component`."
  [ent-id component-id block]
  (state-do
   [:bind component (get-ent-component ent-id component-id)]
   (block component)))

(defn assoc-component
  "For components which are maps, this allows you to operate ASSOC
  semantics directly on the component value."
  [ent-id component-id & args]
  (state-do
   [:bind component (get-ent-component ent-id component-id)]
   (set-ent-component ent-id component-id (apply assoc component-id args))))

(defn dip-component
  "Uses TRANSFORM, a regular function which recieves the old component value
  for COMPONENT-ID from the ENTITY-ID and returns a new value, to
  update the entity's component.

  If your transform function needs to side effect in the state monad,
  then use state-dip-component."
  [ent-id component-id transform]
  (state-do
   [:bind component (get-ent-component ent-id component-id)]
   (set-ent-component ent-id component-id (transform component))))

(defn state-dip-component
  "Uses TRANSFORM, a state function which recieves the old component value
  for COMPONENT-ID from the ENTITY-ID and returns a new value, to
  update the entity's component.

  If your transform is a pure function with respect to the state
  monad, you can use dip-component."
  [ent-id component-id transform]
  (state-do
   [:bind
    component (get-ent-component ent-id component-id)
    result (transform component)]
   (set-ent-component ent-id component-id result)))

(defn handle-systems-for-entity
  "Given an entity ID, this function iterates through the systems and
  adds the entity to systems which match the components of the entity."
  [ent-id]
  (state-do
   [:bind ent (get-ent ent-id)]
   [:let ent-component-ids (:component-ids ent)]
   [:bind systems (state-get-in :systems)]
   (state-map
    (fn [system-key]
      (if (subset? (-> systems system-key :components) ent-component-ids)
        (do

          (add-entity-to-system ent-id system-key))
        (do
          (state-return nil))))
    (keys systems))))

(defn system-deltas
  "Given a set of components and a new set of components, calculate
  which systems have a member ship change and how.  The result is an
  map whose keys are system ids and whose values are :enter | :exit
  | :no-change where :enter means the entity will enter the system if
  the components are so updated, :exit means they will exit the
  system, and :no-change means nothing happens. "
  [old-components new-components]
  (state-do
   [:bind systems (state-get-in :systems)]
   (state-reduce (fn [acc system-id]
                   (state-do
                    [:bind system-components (state-get-in :systems system-id :components)]
                    [:let
                     old-in (subset? system-components old-components)
                     new-in (subset? system-components new-components)]
                    (state-return
                     (assoc acc system-id
                            (cond
                             (= old-in new-in) :no-change
                             old-in :exiting
                             new-in :entering))))
                   ) (keys systems)
                     {})))

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
   (state-do
    [:bind components (state-get-in :components)]
    (throw (Throwable. (format "Could not add component %s to entity %s (valid components are: %s)." component-id entid (keys components)))))))

(defn get-entry-hook
  "A system may have a function which runs when an entity enters the
  system.  Thisxo function retrieves that function or nil if no such
  exists."
  [system-id]
  (state-get-in :systems system-id :enter))

(defn get-exit-hook
  "A system may have a function which runs when an entity leaves the
  system.  This function retrieves that function or nil if no such
  exists."
  [system-id]
  (state-get-in :systems system-id :exit))

(defn get-every
  "A system may have a `default` behavior, its `every` function.  This
  function retrieves that function if it exists or nil is returned, if
  no such function is present."
  [system-id]
  (state-get-in :systems system-id :every))

(defn get-pre-hook
  "A system may have a hook to run before the every function is
  executed.  This function fetches that hook, or nil if none exists."
  [system-id]
  (state-get-in :systems system-id :pre))

(defn get-post-hook
  "A system may have a hook to run after the every function is
  executed.  This function fetches that hook, or nil if none exists."
  [system-id]
  (state-get-in :systems system-id :post))


(declare system-components)
(declare call-with-components)
(defn maybe-invoke-entry-hook
  "Invokes the entry function for a system on an entity, if the entry function exists."
  [ent-id system-id]
  (state-do
   [:bind
    hook (get-entry-hook system-id)
    components (system-components system-id)]
   (if hook (call-with-components ent-id components hook) (state-return ent-id))))

(defn maybe-invoke-exit-hook
  "Invokes the exit function for a system on an entity, if the entry function exists."
  [ent-id system-id]
  (state-do
   [:bind
    hook (get-exit-hook system-id)
    components (system-components system-id)]
   (if hook (call-with-components ent-id components hook) (state-return ent-id))))

(defn maybe-invoke-pre-hook
  "Invokes the pre hook of a system.  The pre hook of a system is
  invoked before the every hook of a system when the execute-system
  function is called."
  [system-id]
  (state-do
   [:bind hook (get-pre-hook system-id)]
   (if hook (hook) (state-return nil))))

(defn maybe-invoke-post-hook
  "Invokes the post hook of a system.  The post hook of a system is
  invoked before the every hook of a system when the execute-system
  function is called."
  [system-id]
  (state-do
   [:bind hook (get-post-hook system-id)]
   (if hook (hook) (state-return nil))))

(defn add-component [ent-id component-id & args]
  "This function adds a component instance to an entity and assures
  that the entity joins the appropriate systems."
  (state-do
   [:bind old-components (get-ent-component-ids ent-id)]
   [:let new-components (conj old-components component-id)]
   [:bind deltas (system-deltas old-components new-components)]
   (apply add-component-raw ent-id component-id args)
   (state-map (fn [system-id]
                (state-do
                 [:let val (system-id deltas)]
                 (case val
                   :entering
                   (state-do
                    (add-entity-to-system ent-id system-id)
                    (maybe-invoke-entry-hook ent-id system-id))
                   :exiting
                   (state-do
                    (maybe-invoke-exit-hook ent-id system-id)
                    (remove-entity-from-system ent-id system-id))
                   :no-change (state-return nil)))) (keys deltas))))

(defn system-for-each
  "Call the function STATE-FUN on each member of the system SYSTEM-ID,
  for side effects in the state monad."
  [system-id state-fun]
  (state-do
   [:bind
    head (first-element-of-system system-id)
    state extract-state]
   (loop [current head
          state state]
     (if (nil? current) (state-do
                         (set-state state)
                         (state-return nil))
         (let [[next new-state]
               ((state-do
                 [:bind next-before (next-entity current system-id)]
                 (state-fun current)
                 [:bind next-after (next-entity current system-id)]
                 (state-return (or next-after next-before)))
                state)]
           (recur next new-state))))))

(defn system-reduce
  "Call the reduction function STATE-FUN-REDUCER on each member of the
  system SYSTEM-ID and the previous value of the reduction function,
  finally, return into the state monad the result of the reduction.

  STATE-FUN-REDUCER must have the type:

  PREVIOUS-VALUE ENT -> (STATE -> [RESULT, STATE])

  "
  [system-id state-reducer init]
  (state-do
   [:bind
    head (first-element-of-system system-id)
    state extract-state]
   (loop [current head
          acc init
          state state]
     (if (nil? current) (state-do (set-state state)
                                  (state-return acc))
         (let [[[new-acc next-ent] new-state]
               ((state-do
                 [:bind
                  next-ent-before-reducer (next-entity current system-id)
                  new-acc (state-reducer acc current)
                  next-ent (or (next-entity current system-id) next-ent-before-reducer)]
                 (state-return [new-acc next-ent]))
                state)]
           (recur next-ent new-acc new-state))))))

(defn system-filter
  "Given a system designator and a filter function (in the state
  monad) return just the entities in the system which cause the
  function to return true."
  [system-id f]
  (system-reduce
   system-id
   (fn [acc it]
     (state-do
      [:bind b (f it)]
      (state-return (if b (conj acc it) acc))))
   []))

(declare system-components)
(declare call-with-components)
(declare call-with-accumulator-and-components)

(defn system-reduce-with-components
  "Call the reduction function STATE-FUN-REDUCER on each member of the
  system SYSTEM-ID and the previous reduction value, beginning with INIT.

  The reduction function should have the signature:

  PREVIOUS-VALUE COMPONENT1 ... COMPONENTN ENT -> (STATE -> [RESULT, STATE])

  "
  [system-id state-reducer init]
  (state-do
   [:bind components (system-components system-id)]
   (system-reduce system-id
                  (fn [acc ent]
                    (call-with-accumulator-and-components acc ent components state-reducer))
                  init)))

(defn system-map
  "Map a curriend state function F across all the entities in a
  system, provided as entity IDs.  F must accept the current entity
  and return a state function, which is executed against the state."
  [system-id f]
  (system-reduce
   system-id
   (fn [acc item]
     (state-do
      [:bind res (f item)]
      (state-return (conj acc res))))
   []))

(defn system-map-with-components
  "Map a curried state-function expecting the components of SYSTEM-ID
  across the entities in the system, collecting the values and
  side-effecting the state as implied in F.  F must take a number of
  arguments equal to the components in the system plus 1.  The
  components are passed in as they were ordered in the system
  definition."
  [system-id f]
  (state-do
   [:bind components (system-components system-id)]
   (system-map system-id
               (fn [ent]
                 (call-with-components ent components f)))))

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

(defn remove-component-raw
  "Removes a component from an entity without updating any information
  about the systems it may or may not now be in."
  [entid component-id]
  (state-do
   [:bind ent (get-ent entid)]
   (set-ent (remove-component-from-entity ent component-id))))

(defn get-components
  "Fetch, as a list, all the components for ENT-ID listed in COMPONENT-IDS."
  [ent-id component-ids]
  (state-map (fn [component-id]
               (state-do
                [:bind entities (state-get-in :entities)]
                (state-return (component-id (:components (entities ent-id))))))
             component-ids))

(defn call-with-components
  "F should be a function of (+ 1 (count component-ids)) arguments
  which is called with the component ids and the entity.  Useful to
  quickly call a function on an entity with components unpacked."
  [ent-id component-ids f]
  (state-bind (get-components ent-id component-ids) #(apply f (conj % ent-id))))

(defn call-with-accumulator-and-components
  "F should be a function of (+ 2 (count component-ids)) arguments
  which is called with an accumulator, the component ids and the entity.  Useful to
  quickly call a function on an entity with components unpacked.
  "
  [acc ent-id component-ids f]
  (state-bind (get-components ent-id component-ids) #(apply f acc (conj % ent-id))))

(defn system-components
  "Get the set of components which makes up a system.  Can be used
  with CALL-WITH-COMPONENTS to quickly operate on the components of a
  system for an entity."
  [system-id]
  (state-get-in :systems system-id :components-order))

(defn call-on-system
  "Given a function F which takes an argument for each component of
  the system and one more for the entity itself, call F for all
  entities in the system with the appropriate components."
  [system-id f]
  (state-do
   [:bind components (system-components system-id)]
   (system-for-each system-id #(call-with-components % components f))))

(defn execute-system
  "CALL-ON-SYSTEM which simply invokes the EVERY function of the
  system, if such exists."
  [system-id]
  (state-do
   (maybe-invoke-pre-hook system-id)
   (state-bind (get-every system-id)
               #(call-on-system system-id %))
   (maybe-invoke-post-hook system-id)))

(defn remove-component
  "Removes a component with a given COMPONENT-ID from an entity with a
  given ENTITY-ID. This is useful in order to modify entities. For
  example, making an entity mortal by adding health."
  [ent-id component-id]
  (state-do
   [:bind old-components (get-ent-component-ids ent-id)]
   [:let new-components (difference old-components #{component-id})]
   [:bind deltas (system-deltas old-components new-components)]
   (state-map (fn [system-id]
                (state-do
                 [:let val (system-id deltas)]
                 (case val
                   :entering
                   (state-do
                    (add-entity-to-system ent-id system-id)
                    (maybe-invoke-entry-hook ent-id system-id))
                   :exiting
                   (state-do
                    (maybe-invoke-exit-hook ent-id system-id)
                    (remove-entity-from-system ent-id system-id))
                   :no-change (state-return nil)))) (keys deltas))))

(defn delete [ent-id]
  "Removes a given entity ID from the current state."
  (state-do
   [:bind
    old-components (get-ent-component-ids ent-id)
    deltas (system-deltas old-components #{})]
   (state-map (fn [system-id]
                (state-do
                 [:let val (system-id deltas)]
                 (case val
                   :exiting
                   (state-do
                    (maybe-invoke-exit-hook ent-id system-id)
                    (remove-entity-from-system ent-id system-id))
                   :no-change (state-return nil)))) (keys deltas))
   (state-dip [:entities]
              #(dissoc % ent-id))))
