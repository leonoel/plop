(ns plop.core
  (:require
    [plop.jvm :as jvm]
    [plop.gcc :as gcc]))

(defn- genstruct [slots env]
  (if (:js-globals env)
    (gcc/genstruct slots)
    (jvm/genstruct slots)))

(defn- walk [f env & syms]
  (if (:js-globals env)
    (gcc/walk f (update env :locals into (map (fn [sym] [sym {:name sym}])) syms))
    (jvm/walk f (into #{} syms))))

(defmacro
  ^{:doc "Defines a set of lexically scoped variables. Variables are bound to optionally type-hinted symbols, hold a reference to a value, are implicitly dereferenced each time they are accessed, and are assignable with set!.
The state of variables is not synchronized. This is a dangerous, expert-only feature, and if the implications of unsynchronized mutable state publication are not obvious to you, you should not use this macro.
Syntax :
(place [symbol1 init-expression1
        symbol2 init-expression2
        ,,,]
  body-expression1
  body-expression2
  ,,,)
A location is first created to store the current state of each variable. Then initialization expressions are evaluated sequentially, each result being assigned to its variable. Finally the body is evaluated and the result of last expression is returned.
Binding scope differs from let in that every variable defined in the block is scoped to the whole block. That is, a initialization expression may refer to its own variable or to another variable defined later in the block."}
  place [bindings & body]
  (let [bindings (partition 2 bindings)
        slots (map first bindings)
        struct (genstruct slots &env)
        splace (gensym "place")
        members (zipmap slots (map #(list '. splace (symbol (str "-" %))) slots))
        transform (walk (fn [locals form] (or (and (not (locals form)) (members form)) form)) &env splace)]
    `(let [~splace ~struct]
       ~@(map (fn [slot form] `(set! ~(members slot) ~(transform form)))
              slots (map second bindings))
       (do ~@(map transform body)))))

(defmacro
  ^{:doc "Defines a set of thread local lexically scoped variables."}
  local [bindings & body]
  (if (:js-globals &env)
    `(place bindings ~@body)
    (let [bindings (partition 2 bindings)
          slots (map first bindings)
          struct (genstruct slots &env)
          slocal (gensym "local")
          sgettl (gensym "gettl")
          splace (gensym "place")
          members (zipmap slots (map #(list '. (with-meta (list sgettl) {:tag struct}) (symbol (str "-" %))) slots))
          transform (walk (fn [locals form] (or (and (not (locals form)) (members form)) form)) &env slocal sgettl splace)]
      `(let [~slocal (ThreadLocal.)]
         (letfn [(~sgettl []
                   (or (.get ~slocal)
                       (let [~splace ~struct]
                         (.set ~slocal ~splace)
                         ~@(map (fn [[slot init]]
                                  `(set! (. ~splace ~(symbol (str "-" slot))) ~(transform init)))
                                bindings)
                         ~splace)))]
           (do ~@(map transform body)))))))