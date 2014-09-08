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
         [[:bind pattern expr] & rest]
         `(state-bind ~expr (fn [~pattern]
                              (state-do ~@rest)))
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

;; (defn state-assoc-in [keys val]
;;   (fn [state]
;;     [val (assoc-in state keys val)]))

;; (defn state-get [& keys]
;;   (fn [state]
;;     [(loop [val state
;;             keys keys]
;;        (if (or (empty? keys) (nil? val))
;;          val
;;          (recur ((first keys) val)
;;                 (rest keys))))
;;      state]))

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
