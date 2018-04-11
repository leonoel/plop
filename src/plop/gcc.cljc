(ns plop.gcc
  (:refer-clojure :exclude [macroexpand])
  (:require [cljs.analyzer :as ana]))

(defn genstruct [slots]
  (let [t (with-meta (gensym "struct__") {:anonymous true})]
    `(do
       (when-not (cljs.core/exists? ~(symbol (str *ns*) (str t)))
         (deftype* ~t ~slots nil nil))
       ~(list* 'new t (repeat (count slots) nil)))))

(defn- macroexpand [env form]
  (if (seq? form)
    (let [exp (ana/macroexpand-1 env form)]
      (if (identical? exp form) form (recur env exp))) form))

(defn- local [name]
  [name {:name name}])

(defn- field [name]
  [name {:name name :field true}])

(defn walk
  ([f e]
   (fn rec [form]
     (let [form (macroexpand e form)]
       (f (into #{} (keys (:locals e)))
          (if (seq? form)
            (let [[op & args] form]
              (with-meta
                (case op
                  (quote ns ns* var)
                  form

                  (if throw do recur new set!)
                  (cons op (map rec args))

                  (.)
                  (list* op (rec (first args)) (second args) (map rec (nnext args)))

                  (js*)
                  (list* op (first args) (map rec (next args)))

                  (def)
                  (let [[name & body] args]
                    (if-some [[init] body]
                      (list op name ((walk f (update e :locals conj (local name))) init))
                      form))

                  (case*)
                  (let [[sym tests thens default & args] args]
                    (list* op (rec sym) tests
                           (into [] (map rec) thens)
                           (rec default) args))

                  (try)
                  (cons op
                        (map (fn [form]
                               (if (seq? form)
                                 (let [[op & args] form]
                                   (case op
                                     catch (let [[type name & body] args]
                                             (list* op type name
                                                    (map (walk f (update e :locals conj (local name))) body)))
                                     finally (cons op (map rec args))
                                     (rec form)))
                                 (rec form)))
                             args))

                  (let* loop*)
                  (let [[e b] (reduce (fn [[e b] [name form]]
                                        [(update e :locals conj (local name))
                                         (conj b name ((walk f e) form))])
                                      [e []] (partition 2 (first args)))]
                    (list* op b (map (walk f e) (next args))))

                  (fn*)
                  (let [name (when (symbol? (first args)) (first args))
                        body (if name (rest args) args)
                        defs (if (vector? (first body)) (list body) body)
                        env (if name (update e :locals conj (local name)) e)]
                    (cons op
                          (concat (when name (list name))
                                  (map (fn [[args & exprs]]
                                         (->> exprs
                                              (map (walk f (update env :locals into
                                                                   (comp (remove #{'&}) (map local)) args)))
                                              (cons args))) defs))))

                  (letfn*)
                  (let [bindings (partition 2 (first args))
                        names (map first bindings)
                        meths (map second bindings)
                        walk (walk f (update e :locals into (map local) names))]
                    (list* op
                           (vec (interleave names (map walk meths)))
                           (map walk (next args))))

                  (deftype* defrecord*)
                  (let [[sym fields masks body & args] args]
                    (list* op sym fields masks
                           ((walk f (assoc e :locals (into {} (map field)
                                                           (concat fields (when (= sym 'defrecord*)
                                                                            '[__meta __extmap __hash]))))) body)
                           args))

                  (map rec form)) (meta form)))
            (if (coll? form)
              (-> (or (empty form) [])
                  (into (map rec) form)
                  (with-meta (meta form))) form)))))))
