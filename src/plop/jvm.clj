(ns plop.jvm
  (:require [clojure.string :as s])
  (:import (clojure.asm Type ClassWriter Opcodes)
           (clojure.asm.commons Method GeneratorAdapter)
           (clojure.lang DynamicClassLoader Compiler$HostExpr)))

(defn- struct-bytecode [cname slots]
  (let [objtype (Type/getType ^Class Object)
        cv (new ClassWriter (. ClassWriter COMPUTE_MAXS))]
    (. cv (visit (. Opcodes V1_5) (+ (. Opcodes ACC_PUBLIC) (. Opcodes ACC_FINAL))
                 cname nil (. objtype (getInternalName))
                 (into-array String nil)))
    (doseq [slot slots]
      (let [tag (get (meta slot) :tag 'Object)]
        (. cv (visitField (. Opcodes ACC_PUBLIC) (Compiler/munge (name slot))
                          (. (Type/getType ^Class (or (Compiler$HostExpr/maybeSpecialTag tag)
                                                      (Compiler$HostExpr/maybeClass tag true)))
                             getDescriptor)
                          nil nil))))
    (let [method (new Method "<init>" (. Type VOID_TYPE) (into-array Type nil))
          gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) method nil nil cv)]
      (. gen (visitCode))
      (. gen (loadThis))
      (. gen (dup))
      (. gen (loadArgs))
      (. gen (invokeConstructor objtype method))
      (. gen (returnValue))
      (. gen (endMethod)))
    (. cv (visitEnd))
    (. cv (toByteArray))))

(defn genstruct [slots]
  (let [npath (s/split (str *ns*) #"\.")
        spath (conj (pop npath) (str (peek npath) "$" (gensym "struct__")))
        pname (namespace-munge (s/join "." spath))
        cname (namespace-munge (s/join "/" spath))
        bcode (struct-bytecode cname slots)
        sname (with-meta (symbol pname) {:no-doc true})]
    (when *compile-files* (Compiler/writeClassFile cname bcode))
    (intern *ns* sname
            (.defineClass ^DynamicClassLoader (deref Compiler/LOADER)
                          pname bcode nil))
    (list 'new sname)))

(defn walk
  ([f locs]
   (fn rec [form]
     (let [form (macroexpand form)]
       (f locs
          (if (seq? form)
            (let [[op & args] form]
              (case op
                (quote var import*)
                form

                (if throw do recur new set! monitor-enter monitor-exit)
                (cons op (map rec args))

                (.)
                (list* op (rec (first args)) (next args))

                (def)
                (let [[name & body] args]
                  (if-some [[init] body]
                    (list op name ((walk f (conj locs name)) init))
                    form))

                (case*)
                (let [[sym shift mask default imap & args] args]
                  (list* op (rec sym) shift mask (rec default)
                         (into {} (map (fn [[hash [cnst expr]]]
                                         [hash [cnst (rec expr)]]))
                               imap) args))

                (try)
                (cons op
                      (map (fn [form]
                             (if (seq? form)
                               (let [[op & args] form]
                                 (case op
                                   catch (let [[type name & body] args]
                                           (list* op type name (map (walk f (conj locs name)) body)))
                                   finally (cons op (map rec args))
                                   (rec form)))
                               (rec form)))
                           args))

                (let* loop*)
                (let [[s b] (reduce (fn [[s b] [symbol form]]
                                      [(conj s symbol) (conj b symbol ((walk f s) form))])
                                    [locs []] (partition 2 (first args)))]
                  (list* op b (map (walk f s) (next args))))

                (fn*)
                (let [name (when (symbol? (first args)) (first args))
                      body (if name (rest args) args)
                      defs (if (vector? (first body)) (list body) body)
                      locs (if name (conj locs name) locs)]
                  (concat (cons op (when name (list name)))
                          (map (fn [[args & exprs]]
                                 (cons args (map (walk f (into locs (remove #{'&}) args)) exprs)))
                               defs)))

                (letfn*)
                (let [bindings (partition 2 (first args))
                      names (map first bindings)
                      meths (map second bindings)
                      walk (walk f (into locs names))]
                  (list* op
                         (vec (interleave names (map walk meths)))
                         (map walk (next args))))

                (deftype*)
                (concat (cons op (take 5 args))
                        (map (fn [[name args & body]]
                               (list* name args (map (walk f (into locs args)) body)))
                             (drop 5 args)))

                (reify*)
                (list* op (first args)
                       (map (fn [[name args & body]]
                              (list* name args (map (walk f (into locs args)) body)))
                            (next args)))

                (map rec form)))
            (if (coll? form) (into (or (empty form) []) (map rec) form) form)))))))