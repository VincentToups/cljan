(ns cljan.core-test
  (:require [clojure.test :refer :all]
            [cljan.core :refer :all]
            [cljan.state-monad :refer :all]))

(deftest system-entities-snapshot-preserves-state 
  (testing "requesting the list of system entities should not delete the state."
    (is (not= nil (second 
                 (run-cljan
                  (component :c1 identity)
                  (component :c2 identity)
                  (system :s1 [:c1]
                          {
                           :every identity
                           })
                  (system-entities-snapshot :s1)))))))

(deftest test-system-deltas 
  (testing "entering a system with one component"
    (is (= {:s1 :entering} (first 
               (run-cljan
                (component :c1 identity)
                (component :c2 identity)
                (system :s1 [:c1]
                        {
                         :every identity
                         })
                (system-deltas #{} #{:c1})))))
    (is (= {:s1 :entering} (first 
                            (run-cljan
                             (component :c1 identity)
                             (component :c2 identity)
                             (system :s1 [:c1]
                                     {
                                      :every identity
                                      })
                             (system-deltas #{:c2} #{:c2 :c1}))))))
  (testing "exiting a system with one component"
    (is (= {:s1 :exiting} (first 
                            (run-cljan
                             (component :c1 identity)
                             (component :c2 identity)
                             (system :s1 [:c1]
                                     {
                                      :every identity
                                      })
                             [:bind e1 (entity)]
                             (add-component e1 :c1 10)
                             (system-deltas #{:c1} #{})))))
    (is (= {:s1 :exiting} (first 
                            (run-cljan
                             (component :c1 identity)
                             (component :c2 identity)
                             (system :s1 [:c1]
                                     {
                                      :every identity
                                      })
                             [:bind e1 (entity)]
                             (add-component e1 :c1 10)
                             (system-deltas #{:c2 :c1} #{:c2})))))))

(deftest removing-component-from-entity
  (testing "If the component is removed from the members of an entity"
    (is (= (@#'cljan.core/remove-component-from-entity
            {:component-ids #{:c2 :c1}
             :components    {:c2 34 :c1 10}}
            :c2)
           {:component-ids #{:c1}
            :components    {:c1 10}})))
  (testing "removing a component from a entity without components."
    (let [entities (:entities 
                 (second (run-cljan
                         (component :c1 identity)
                         (system :s1 [:c1]
                                 {
                                  :every identity
                                  })
                         [:bind e1 (entity)]
                         [:aside (print e1)]
                         (remove-component e1 :c1))))]
      (is (= entities
             {0 {:id 0 :component-ids #{}, :components {} :nexts {} :prevs {}}}))))
  (testing "Removing a component from an entity that would make the entity fall out of a system."
    (let [system-s1-entities
          (-> (run-cljan
                     (component :c1 identity)
                     (component :c2 identity)
                     (component :c3 identity)
                     (system :s1 [:c1 :c2]
                             {
                              :every identity
                              })
                     [:bind e1 (entity)]
                     (add-component e1 :c1 10)
                     (add-component e1 :c2 34)
                     (remove-component e1 :c2)
                     (system-entities-snapshot :s1))
                    first)]
      (is (= system-s1-entities [])))))

(deftest test-constructing-new-cljan
  (testing "Test creating a new cljan."
    (is (=
         {:components     {}
          :systems        {}
          :entities       {}
          :entity-id-counter 0}
         (make-cljan)))))

(deftest test-component-detection
  (testing "The creation and look up of components"
    (is (= false
           (run-cljan*
            (component :c1 (fn [a] a))
            (component? :c2))))
    (is (= true
           (run-cljan*
            (component :c1 (fn [a] a))
            (component? :c1))))
    (is (= false
           (run-cljan*
            (component :c1 (fn [a] a))
            (component :c2 (fn [a] a))
            (components? :c1 :c2 :c3))))
    (is (= true
           (run-cljan*
            (component :c1 (fn [a] a))
            (component :c2 (fn [a] a))
            (components? :c1 :c2))))))

(deftest test-component-creation
  (testing "The creation of components and their retrieval."
    (is (= (-> (run-cljan
                (component :c1 (fn [a] a)))
               second
               :components
               :c1 :name)
           :c1))
    (is (= (-> (run-cljan
                (component :c1 (fn [a] a))
                (component :c2 (fn [b] b)))
               second
               :components
               keys
               sort)
           [:c1 :c2]))))

(deftest test-entity-creation
  (testing "The creation of entities and their retrieval."
    (is (= (-> (run-cljan (entity))
               second
               :entities
               count)
           1))
    (is (= (-> (run-cljan (entity)
                          (entity)
                          (entity)
                          (entity))
               second
               :entities
               count)
           4))
    ;; (is (= (:entities (run-cljan*
    ;;                    (entity)))
    ;;        {0 {}}))

    ))

(deftest test-system-creation
  (testing "Test creation of systems."
    (is (let [f (fn [a] a)]
          (= (-> (run-cljan
                  (component :c1 (fn [a] a))
                  (component :c2 (fn [b] b))
                  (system :s1 [:c1 :c2]
                          {
                           :every f
                           }))
                 second
                 :systems
                 {:s1 {:components [:c1 :c2]
                       :every f
                       }}))))))

(deftest test-entities-added-to-appropriate-systems
  (testing "Test that entities are added to the right systems when components are added."
    (is (= (-> (run-cljan
                (component :c1 identity)
                (component :c2 identity)
                (system :s1 [:c1 :c2]
                        {
                         :every identity
                         })
                [:bind e1 (entity)]
                (add-component e1 :c1 10)
                (add-component e1 :c2 34)
                (system-entities-snapshot :s1))
               first)
           [0]))
    (is (= (-> (run-cljan
                (component :c1 identity)
                (component :c2 identity)
                (component :c3 identity)
                (system :s1 [:c1 :c2]
                        {
                         :every identity
                         })
                [:bind e1 (entity)]
                (add-component e1 :c1 10)
                (add-component e1 :c2 34)
                (add-component e1 :c3 0)
                (system-entities-snapshot :s1))
               first)
           [0]))))

(deftest test-entities-are-not-added-to-systems-they-dont-want-to-be-in
  (testing "Test that entities are not erroneously put into non-matching systems"
    (is (= (-> (run-cljan
                (component :c1 identity)
                (component :c2 identity)
                (system :s1 [:c1 :c2]
                        {
                         :every identity
                         })
                [:bind e1 (entity)]
                (add-component e1 :c1 10)
                (system-entities-snapshot :s1))
               first)
           []))))

(deftest reduction-over-a-system []
  (testing "The usage of system-reduce-with-components."
    (let [init-components (fn []
                            (state-do
                             (component :value)
                             (component :incr)))
          growing-values-every (fn [value incr ent]
                                 (set-ent-component ent :value (+ value incr)))
          init-systems (fn []
                         (system :growing-values [:value :incr]
                                 {:every growing-values-every}))
          init-entities (fn []
                          (state-do
                           (entity 
                            [:value 0]
                            [:incr 10])
                           (entity
                            [:value 0]
                            [:incr 11])))
          get-value (fn []
                       (system-reduce-with-components
                        :growing-values 
                        (fn [acc value incr ent]
                          (state-return (+ acc value)))
                        0))]
      (is (= 42 (run-cljan* 
                      (init-components)
                      (init-systems)
                      (init-entities)
                      (execute-system :growing-values)
                      (execute-system :growing-values)
                      (get-value)))))))

(deftest test-invoking-hooks
  (testing "Whether exit hook on system works when an entity exits a system but is not deleted."
    (let [init-components (fn []
                            (state-do
                             (component :value)
                             (component :incr)))
          growing-values-exit (fn [ent]
                               (state-assoc :exit-called ent))
          growing-values-every (fn [value incr ent]
                                 (set-ent-component ent :value (+ value incr)))
          init-systems (fn []
                         (system :growing-values [:value :incr]
                                 {:every growing-values-every :exit growing-values-exit}))
          init-entities (fn []
                          (state-do
                           (entity 
                            [:value 0]
                            [:incr 10])
                           [:bind stored-ent (entity
                             [:value 0]
                             [:incr 11])]
                           (state-assoc :stored-ent stored-ent)))
          get-value (fn []
                      (state-get :exit-called))]
      (is (= 1 (run-cljan* 
                      (init-components)
                      (init-systems)
                      (init-entities)
                      (execute-system :growing-values)
                      [:bind e (state-get :stored-ent)]
                      (remove-component e :incr)
                      (get-value))))))
  (testing "Whether exit hook on system works when an entity is deleted."
    (let [init-components (fn []
                            (state-do
                             (component :value)
                             (component :incr)))
          growing-values-exit (fn [ent]
                               (state-assoc :exit-called ent))
          growing-values-every (fn [value incr ent]
                                 (set-ent-component ent :value (+ value incr)))
          init-systems (fn []
                         (system :growing-values [:value :incr]
                                 {:every growing-values-every :exit growing-values-exit}))
          init-entities (fn []
                          (state-do
                           (entity 
                            [:value 0]
                            [:incr 10])
                           [:bind stored-ent (entity
                             [:value 0]
                             [:incr 11])]
                           (state-assoc :stored-ent stored-ent)))
          get-value (fn []
                      (state-get :exit-called))]
      (is (= 1 (run-cljan* 
                      (init-components)
                      (init-systems)
                      (init-entities)
                      (execute-system :growing-values)
                      [:bind e (state-get :stored-ent)]
                      (delete e)
                      (get-value))))))
  (testing "Whether entry hook on system works."
    (let [init-components (fn []
                            (state-do
                             (component :value)
                             (component :incr)))
          growing-values-enter (fn [ent]
                               (state-assoc :enter-called ent))
          growing-values-every (fn [value incr ent]
                                 (set-ent-component ent :value (+ value incr)))
          init-systems (fn []
                         (system :growing-values [:value :incr]
                                 {:every growing-values-every :enter growing-values-enter}))
          init-entities (fn []
                          (state-do
                           (entity 
                            [:value 0]
                            [:incr 10])
                           (entity
                            [:value 0]
                            [:incr 11])))
          get-value (fn []
                      (state-get :enter-called))]
      (is (= 1 (run-cljan* 
                      (init-components)
                      (init-systems)
                      (init-entities)
                      (execute-system :growing-values)
                      (get-value))))))
  (testing "Whether invoking post on a system works."
    (let [init-components (fn []
                            (state-do
                             (component :value)
                             (component :incr)))
          growing-values-post (fn []
                               (state-assoc :post-called true))
          growing-values-every (fn [value incr ent]
                                 (set-ent-component ent :value (+ value incr)))
          init-systems (fn []
                         (system :growing-values [:value :incr]
                                 {:every growing-values-every :post growing-values-post}))
          init-entities (fn []
                          (state-do
                           (entity 
                            [:value 0]
                            [:incr 10])
                           (entity
                            [:value 0]
                            [:incr 11])))
          get-value (fn []
                      (state-get :post-called))]
      (is (= true (run-cljan* 
                      (init-components)
                      (init-systems)
                      (init-entities)
                      (execute-system :growing-values)
                      (get-value))))))
  (testing "Whether invoking pre on a system works."
    (let [init-components (fn []
                            (state-do
                             (component :value)
                             (component :incr)))
          growing-values-pre (fn []
                               (state-assoc :pre-called true))
          growing-values-every (fn [value incr ent]
                                 (set-ent-component ent :value (+ value incr)))
          init-systems (fn []
                         (system :growing-values [:value :incr]
                                 {:every growing-values-every :pre growing-values-pre}))
          init-entities (fn []
                          (state-do
                           (entity 
                            [:value 0]
                            [:incr 10])
                           (entity
                            [:value 0]
                            [:incr 11])))
          get-value (fn []
                      (state-get :pre-called))]
      (is (= true (run-cljan* 
                      (init-components)
                      (init-systems)
                      (init-entities)
                      (execute-system :growing-values)
                      (get-value))))))
  (testing "Whether invoking every on a system works."
    (let [init-components (fn []
                            (state-do
                             (component :value)
                             (component :incr)))
          growing-values-every (fn [value incr ent]
                                 (set-ent-component ent :value (+ value incr)))
          init-systems (fn []
                         (system :growing-values [:value :incr]
                                 {:every growing-values-every}))
          init-entities (fn []
                          (state-do
                           (entity 
                            [:value 0]
                            [:incr 10])
                           (entity
                            [:value 0]
                            [:incr 11])))
          get-values (fn []
                       (system-map-with-components
                        :growing-values 
                        (fn [value incr ent]
                          (state-return value))))]
      (is (= [20 22] (run-cljan* 
                      (init-components)
                      (init-systems)
                      (init-entities)
                      (execute-system :growing-values)
                      (execute-system :growing-values)
                      (get-values)))))))


(run-tests)



