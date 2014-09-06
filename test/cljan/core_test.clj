(ns cljan.core-test
  (:require [clojure.test :refer :all]
            [cljan.core :refer :all]))

(deftest test-constructing-new-cljan
  (testing "Test creating a new cljan."
    (is (= 
         {:components {}
          :systems {}
          :entities []}
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
               :components) 
           {:c1 {:name :c1}}))
    (is (= (-> (run-cljan 
                (component :c1 (fn [a] a))
                (component :c2 (fn [b] b)))
               second
               :components) 
           {:c1 {:name :c1}
            :c2 {:name :c2}}))))

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


;; (deftest ecs-test-1 
;;   (testing "Test entities, components, systems."
;;     (let [{:keys [components systems entities]} 
;;           (run-cljan 
;;            [health (component (fn [amt] amt))
;;             _ (system :components [health] 
;;                       :every (fn [amt ent]
;;                                (if (< amt 0)
;;                                  (delete ent)
;;                                  (state-return nil))))
;;             ent (entity)
;;             _ (entity-add ent health 0)] (run-all))]
;;       (is (= 1 (count components)))
;;       (is (= 1 (count entities)))
;;       (is (= 1 (count systems))))))

;; (deftest run-systems-test-1
;;   (testing "Test the implications of running one system."
;;     (let [{:keys [components systems entities]} 
;;           (run-cljan 
;;            [health (component (fn [amt] amt))
;;             _ (system :components [health] 
;;                       :every (fn [amt ent]
;;                                (if (< amt 0)
;;                                  (delete ent)
;;                                  (state-return nil))))
;;             ent (entity)
;;             _ (entity-add ent health 0)] (run-all))]
;;       (is (= 1 (count components)))
;;       (is (= 1 (count entities)))
;;       (is (= 1 (count systems))))))

(run-tests)
