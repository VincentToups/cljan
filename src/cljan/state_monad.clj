(ns cljan.state-monad
  (:require [clojure.core.typed :refer [ann All HVec U Any Map Seq Coll Option]
             :as typed]
            [clojure.core.match :refer [match]]))

(t/ann state-return (All [X Y] [X -> [Y -> (HVec [X Y])]]))
(defn state-return [x]
  (fn [state]
    [x state]))

(ann state-bind
     (All [state x y]
          [[state -> (HVec [x state])]
           [x -> [state -> (HVec [y state])]]
           ->
           [state -> (HVec [y state])]]))
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
     (All [k v]
          [k v -> [(Map k v) -> (HVec [v (Map k v)])]]))
(defn state-assoc [key val]
  (fn [state]
    [val
     (assoc state key val)]))

(ann state-assoc-in
     (All [v]
          [(Seq Any) v -> [(Map Any Any) ->
                           (HVec [v Any])]]))
(defn state-assoc-in [keys val]
  (fn [state]
    [val (assoc-in state keys val)]))

(ann state-get
     [Any *
      ->
      [(U (Option (clojure.lang.ILookup Any Any)) Any)
       ->
       (HVec [(Option Any)
              (U (Option (clojure.lang.ILookup Any Any)) Any)])]])

(defn state-get [& keys]
  (fn [state]
    [(typed/loop [val  :- (U (Option (clojure.lang.ILookup Any Any)) Any)
                  ,  state
                  keys :- (Option (typed/I (typed/CountRange 1)
                                           (typed/HSeq (Any *))))
                  , keys]
       (if (or (empty? keys) (nil? val))
         val
         (recur (get val (first keys))
                (rest keys))))
     state]))

;; (defn extract-state [state]
;;   [state state])

;; (defn set-state [new-state]
;;   (fn [old-state]
;;     [new-state new-state]))

;; (defmacro defstatefn [name args & body]
;;   `(defn ~name ~args (state-do ~@body)))

;; (defn state-map [f collection]
;;   (fn [state]
;;     (loop [result []
;;            state state
;;            collection collection]
;;       (match [collection]
;;              [([] :seq)] [result state]
;;              [([first & rest] :seq)]
;;              (let [[val state] ((f first) state)]
;;                (recur (conj result val)
;;                       state
;;                       rest))))))
