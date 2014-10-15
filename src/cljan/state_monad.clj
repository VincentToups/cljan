(ns cljan.state-monad
  (:require [clojure.core.typed :refer
             [ann All HVec Any Map Coll Option Seq defalias AnyInteger]
             :as t]
            [clojure.core.match :refer [match]]))

(defalias MonadicState
  (HVec [Any Any]))

(defalias MonadicFunctionArity1
  [Any -> [Any -> MonadicState]])

(defalias MonadicFunctionArity2
  [Any Any -> [Any -> MonadicState]])

(ann state-return (All [x y] [x -> [y -> (HVec [x y])]]))
(defn state-return [x]
  (fn [state]
    [x state]))

(ann state-bind
       (All [State X Y]
              [[State -> (HVec [X State])]
               [X -> [State -> (HVec [Y State])]]
               ->
               [State -> (HVec [Y State])]]))

(defn state-bind [mv mf]
  (fn [state]
    (let [[value new-state] (mv state)]
      ((mf value) new-state))))

(defmacro state-do [& forms]
  (match (into [] forms)
         [last-form] last-form
         [[:ifm expr true-branch false-branch] & rest]
         `(state-bind ~expr (fn [x#]
                              (if x#
                                true-branch
                                false-branch)))
         [[:bind pattern expr & rest-bind] & rest]
         (if (or (empty? rest-bind) (nil? rest-bind))
           `(state-bind ~expr (fn [~pattern]
                                (state-do ~@rest)))
           `(state-bind ~expr (fn [~pattern]
                                (state-do [:bind ~@rest-bind] ~@rest))))
         [[:aside & aside-rest] & rest]
         `(do ~@aside-rest (state-do ~@rest))
         [[:let & let-forms] & rest]
         `(let ~let-forms (state-do ~@rest))
         [expr & rest]
         `(state-bind ~expr (fn [y#]
                              (state-do ~@rest)))))

(defmacro state-if [expr true-branch false-branch]
  `(state-bind ~expr
               (fn [x#]
                 (if x#
                   ~true-branch
                   ~false-branch))))

(ann state-assoc
       (All [k v x]
              [k v -> [(Map k v) ->
                       (HVec [v (Map k v)])]]))
(defn state-assoc [key val]
  (fn [state]
    [val
     (assoc state key val)]))

(ann state-assoc-in
     (All [x]
          [(Seq Any) x -> [(Option (Map Any Any)) -> (HVec [x Any])]]))
(defn state-assoc-in [keys val]
  (fn [state]
    [val (assoc-in state keys val)]))

(ann state-get-in
     (All [state]
          [Any * -> [state -> (HVec [Any state])]]))
(defn state-get-in [& keys]
  (fn [state]
    [(get-in state keys)
     state]))

(ann extract-state
     (All [state]
          [state -> (HVec [state state])]))
(defn extract-state [state]
  [state state])

(ann set-state
     (All [state]
          [state -> [state -> (HVec [state state])]]))
(defn set-state [new-state]
  (fn [old-state]
    [new-state new-state]))

(ann state-reduce
     [MonadicFunctionArity2
      (Option (Seq Any))
      Any -> [MonadicState -> Any]])
(defn state-reduce
  "Using F, a state-function which takes an accumulator and an item, reduce
  the sequence COLLECTION starting with the initial value INIT."
  [f collection init]
  (fn [state]
    (t/loop [result     :- Any, init
             state*     :- Any, state
             collection :- (Option (Seq Any)), collection]
      (cond
       (empty? collection) [result state*]
       :otherwise
       (let [first (first collection)
             rest (rest collection)
             [new-result new-state] ((f result first) state*)]
         (recur new-result
                new-state
                rest))))))

(ann state-for-each
     [MonadicFunctionArity1
      (Option (Seq Any))
      -> [MonadicState -> Any]])
(defn state-for-each
  "For each item in collection, call the state-function F on the item."
  [f collection]
  (fn [state]
    (t/loop [state*      :- Any               , state
             collection  :- (Option (Seq Any)), collection]
      (cond
       (empty? collection) [nil state*]
       :otherwise
       (let [first (first collection)
             rest (rest collection)
             [ignored new-state] ((f first) state*)]
         (recur new-state rest))))))

(ann state-repeat
     [AnyInteger MonadicFunctionArity1 ->
      [MonadicState -> Any]])
(defn state-repeat
  "Repeat the parameterized state-function F N times, passing the
  iteration as the first parameter."
  [n f]
  (state-for-each f (range n)))


(ann state-map
     [MonadicFunctionArity1 (Option (Seq Any))
      -> [MonadicState -> Any]])
(defn state-map
  "Map F, a state-function, across the sequence COLLECTION and return
  a COLLECTION of the results."
  [f collection]
  (fn [state]
    (t/loop [result     :- (HVec []), []
             state      :- Any, state
             collection :- (Option (Seq Any)),  collection]
      (cond
       (empty? collection) [result state]
       :otherwise
       (let [first (first collection)
             rest (rest collection)
             [val new-state] ((f first) state)]
         (recur (conj result val) new-state rest))))))


;; (ann state-dip
;;      [(Seq Any) MonadicFunctionArity1 -> MonadicState])
(defn state-dip
  "Given a chain of keys pointing to somewhere in the state, pass that
  value to f and set the value to the return value of f."
  [location f]
  (state-do
   [:bind v (apply state-get-in location)]
   (state-assoc-in location (f v))))
