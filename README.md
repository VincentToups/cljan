# cljan

Cljan (pronounced "Cla-jan") is game-engine independent, purely
functional entity component system inspired by
[paldepind](https://github.com/paldepind)'s
[Kran](https://github.com/paldepind/Kran).

If you know Kran, then Cljan will be familiar to you: they share the
same conceptual underpinning.  As in Kran, one constructs a universe
of interacting _entities_ by defining _components_ which participate
in _systems_.

Cljan provides a state-monadic framework for using these ideas in the
context of Clojure's pure, functional environmet.  Cljan is frame-work
agnostic, allowing you to use it with the system of your choice.

## Usage

We begin with a brief example, but if you are interested in the
concepts underpinning the library, see below for an explanation of the
state monad and of entity component systems.

### Example

We'll now walk through defining a simple example.  

We begin with defining our components.  One always begins with
components, because we build up systems by reference to what
components constitute membership and we construct entities by what
components they have.

Our world will simulate some crops, which have a growth stage and a
growth rate.  Each piece of data will be one component:

    (defn init-components []
      (state-do 
       (component :growth-stage identity)
       (component :growth-rate identity)))

The function `component` creates a component in the Cljan universe.
It takes two arguments: the first is the name of the system and the
second is a constructor.  Each of our components is just a single
number, so our constructors are the identity function, but we may
wish, in general, to perform calculations before returning the
constructed value.

We now create our system:

    (defn grow [growth-stage growth-rate ent]
      (set-ent-component ent :growth-stage (+ growth-rate growth-stage)))

    (defn init-systems []
      (state-do
       (system :growing-things [:growth-stage :growth-rate] 
         {:every grow})))

This code means: "Create a system called `growing things` which
operates on any entities which contain a growth stage and a growth
rate.  Every time this system is executed, update the growth stage by
the growth rate."

Finally, we create some entities:

    (defn init-entities []
      (state-repeat 10 
                    (fn [i]
                      (state-do 
                       [:bind e (entity)]
                       (add-component e :growth-state 0)
                       (add-component e :growth-rate (rand-int
      10))))))

Here we create ten entities and give them random growth rates.  All
that remains is to define our init and update function:

    (defn init []
      (state-do 
       (init-components)
       (init-systems)
       (init-entities)))

    (defn update []
      (execute-system :growing-things))

Of course, eventually we must initialize and update our systems.
Cljan provides a special form to run Cljan code on a fresh Cljan
state and to update a cljan state with a state-function:

    (def world (atom (run-cljan-for-state (init))))

    (defn update-world! []
      (swap! world (fn [world]
                     (update-cljan world (update)))))

    (loop [i 0]
      (if (< i 10) (do (update-world!) (recur (+ i 1))) @world))

Here we initialize our Cljan state inside an atom and update it ten
times.  We can then extract the growth stage of each entity and see
how they have developed:

    (map (fn [key] 
           (-> ((:entities @world) key) :components :growth-stage)) 
         (keys (:entities @world)))

    ; -> (0 140 140 120 40 180 20 120 40 160)

And that is the whole story!


### Background

#### Concepts

To use Cljan effectively, it is important to digest three concepts:

*   components
*   systems
*   entities

They relate in the following way: Entities are containers for
components, which are pieces of data like "hit points" or "position".
Systems collect sets of entities when they have certain combinations
of components.  

When using Cljan, one first declares the components that will be used
in the Cljan-universe.  Then one declares the systems that exist in
terms of the components.

Finally, before and during the game, the programmer creates entities.
During the course of the game, components are added and removed from
entities, which in turn causes entities to be part of, or to not be
part of systems.  The systems, for their part, define the behavior of
the entities in the game.

#### The State Monad

Cljan is entirely pure and uses a custom state-monad to orchestrate
the sequentially dependent operations that are at the heart of game
logic.

If you don't understand monads, don't worry!  Cljan is easy to use
once you become comfortable with one special form, `state-do`, and one
idea: "state functions".

A state function is a function which takes a single argument, the
current state of the cljan universe, for instance, and returns a
vector of two values.  The second value is the new state, while the
first value any ancillary result of the operation performed by the
function.

For instance:

    (defn attack [state]
      (let [hp (-> state :player :health)
 	        new-state (assoc-in state [:player :health] (- health 1))
            dead? (= 0 (-> state :player :health))]
         [dead? new-state]))

This function attacks a player entitiy held by the state and also
returns a boolean indicating whether that player is dead. 

`state-do` is a special form built for sequencing state-functions so
that future-state functions can operate on the return values, the
first element of the two-element vector, without explicitly passing
around the state, for instance:

    (state-do 
     [:bind dead? attack]
     (if dead? attack end-game))

A `state-do` expression produces a state function which you can
execute by passing a state to.

The body of a `state-do` expression must be a sequence of expressions
of a restricted type.  The simplest case is a Clojure expression
_whose result is a state function_.  In this case, the meaning is to
execute that state-function for its side effect but throw away
"return" value, the first value in the pair returned by function.

The next case is an expression of the form `[:bind pattern expr]`.
The `expr` must evaluate to a state function and the `:bind` for
ensures that the variable bindings implied by the `pattern` are active
in all forms which appear after the bind.  In the above exampe, for
instance, the variable `dead?` is bound to the first element of the
vector returned by `attack`.  Note that `attack` is a state function. 

Easier to understand are forms of the type `[:let pattern expr ...]`,
which introduce regular bindings, not sequenced through the state
monad.

And finally, one may say `[:aside expr ...]`, where any expressions
may be placed in the list of expressions.  They are executed when the
complete state function is called, and can refer to any bindings
introduced above them, but do not side effect the state or bind any
values for subsequent expressions.

`state-do` provides a concise way of sequencing and working with state
functions, such that users of Cljan will almost never need to write
state-functions out manually.

Curried functions serve to allow the programmer to parameterize state
functions.  Imagine, for instance, a parameterized `attack` function:

    (defn attack [attack-power]
      (fn [state] 
        (let [hp (-> state :player :health)
 	        new-state (assoc-in state 
                        [:player :health] (- health attack-power))
            dead? (= 0 (-> state :player :health))]
         [dead? new-state])))

`attack` is now no longer a state function, but it _returns_ one, and
would be used like so:

    (state-do 
     [:bind dead? (attack 5)]
     (if dead? attack end-game))


## On the Implementation

The challenging aspect of porting Kran to Clojure is that Kran is
quite side-effect heavy.  For instance, during the update of an entity
in a system, other entities may be added or removed from that or other
systems.

In Kran this is handled by storing entities in stateful doubly linked
lists, so that the Kran system can modify the entities in a system on
the fly, even during traversal of all entities in a system.

In order to accomplish this in Cljan, the linked lists for every
system needed to either be stored in a stateful way or placed entirely
into the management of the state monad, which is what we decided to
do.

Each entity in the state of the Cljan universe records, for every
system it belongs to, the next entity in that system and the
previous.  All update functions live in the state monad, and therefor
can trigger the addition or removal of entities from these linked
lists.  We preserve, in this way, the stateful semantics of Kran in a
purely functional universe.

Cljan is still alpha software, but we plan to use it to develop our
next game, so it should get a lot of exercise in the coming months.

## License

Copyright © 2014 Vincent Toups and Eduardo Bellani 

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
