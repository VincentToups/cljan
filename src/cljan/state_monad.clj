(ns cljan.state-monad
  (:require [clojure.core.typed :as t]
            [clojure.core.match :refer [match]]))

(t/ann state-return (t/All [X Y] [X -> [Y -> (t/HVec [X Y])]]))
(defn state-return [x]
  (fn [state]
    [x state]))

(t/ann state-bind
       (t/All [State X Y]
              [[State -> (t/HVec [X State])]
               [X -> [State -> (t/HVec [Y State])]]
               ->
               [State -> (t/HVec [Y State])]]))

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

(t/ann state-assoc
       (t/All [k v x]
              [k v -> [(clojure.lang.IPersistentMap k v) ->
                       (t/HVec [v (clojure.lang.IPersistentMap k v)])]]))
(defn state-assoc [key val]
  (fn [state]
    [val
     (assoc state key val)]))

(defn state-assoc-in [keys val]
  (fn [state]
    [val (assoc-in state keys val)]))

(defn state-get [& keys]
  (fn [state]
    [(loop [val state
            keys keys]
       (if (or (empty? keys) (nil? val))
         val
         (recur (val (first keys))
                (rest keys))))
     state]))

(defn extract-state [state]
  [state state])

(defn set-state [new-state]
  (fn [old-state]
    [new-state new-state]))

(defmacro defstatefn [name args & body]
  `(defn ~name ~args (state-do ~@body)))

(defn state-reduce 
  "Using F, a state-function which takes an accumulator and an item, reduce
  the sequence COLLECTION starting with the initial value INIT."
  [f collection init]
  (fn [state]
    (loop [result init
           state state
           collection collection]
      (cond 
       (empty? collection) [result state]
       :otherwise 
       (let [first (first collection)
             rest (rest collection)
             [new-result new-state] ((f result first) state)]
         (recur new-result new-state rest))))))

(defn state-for-each
  "For each item in collection, call the state-function F on the item."
  [f collection]
  (fn [state]
    (loop [state state
           collection collection]
      (cond 
       (empty? collection) [nil state]
       :otherwise 
       (let [first (first collection)
             rest (rest collection)
             [ignored new-state] ((f first) state)]
         (recur new-state rest))))))

(defn state-map 
  "Map F, a state-function, across the sequence COLLECTION and return
  a COLLECTION of the results."
  [f collection]
  (fn [state]
    (loop [result []
           state state
           collection collection]
      (cond 
       (empty? collection) [result state]
       :otherwise 
       (let [first (first collection)
             rest (rest collection)
             [val new-state] ((f first) state)]
         (recur (conj result val) new-state rest))))))
