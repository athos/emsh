(ns emsh.compile
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]))

(defn lookup [{:keys [locals]} sym]
  (or (get locals sym)
      (resolve '{do do} sym)
      (when (and (nil? (namespace sym))
                 (not= sym 'do))
        (resolve (symbol "emsh.core" (name sym))))))

(defn- as-expr [cenv] (assoc cenv :context :expr))
(defn- as-statement [cenv] (assoc cenv :context :statement))

(defn- inherit-context [cenv' cenv]
  (assoc cenv' :context (:context cenv)))

(defmulti ^:private compile* (fn [cenv form] (first form)))

(defn- wrap-with-coerce-fn [cenv form]
  (if-let [coerce-fn (get {:expr `emsh.core/->str
                           :statement `emsh.core/wait-for
                           :conditional `emsh.core/succeeded?}
                          (:context cenv))]
    `(~coerce-fn ~form)
    form))

(declare compile)

(defmacro expand-and-compile [ctx m form]
  (let [cenv {:locals &env :context ctx}
        do-macro (resolve 'emsh.core/do)
        expand (fn [v form]
                 (let [form' (apply v (cond-> form
                                        ;; if macro to be expanded is emsh.core/do,
                                        ;; inject context info to its metadata
                                        ;; for more composability
                                        (= v do-macro)
                                        (vary-meta assoc :context ctx))
                                    &env (rest form))]
                   (if-let [v' (and (seq? form')
                                    (symbol? (first form'))
                                    (lookup cenv (first form')))]
                     (if (and (var? v') (:macro (meta v')))
                       (recur v' form')
                       form')
                     form')))]
    (compile cenv (expand (resolve m) form))))

(defn- compile-seq [cenv [op & args :as form]]
  (if-let [v (and (symbol? op) (lookup cenv op))]
    (or (when (var? v)
          (cond (:macro (meta v))
                `(expand-and-compile ~(:context cenv) ~(symbol v) ~form)

                (or (:process-in (meta v))
                    (:process-out (meta v)))
                (let [ctx (if (:process-in (meta v)) :emsh :expr)
                      cenv' (assoc cenv :context ctx)]
                  (cond->> (with-meta
                             `(~(symbol v) ~@(map (partial compile cenv') args))
                             (meta form))
                    (:process-out (meta v))
                    (wrap-with-coerce-fn cenv)))))
        (with-meta
          `(~op ~@(map (partial compile (as-expr cenv)) args))
          (meta form)))
    (with-meta
      (cond (and (symbol? op)
                 (or (special-symbol? op)
                     (str/starts-with? (name op) ".")
                     (str/ends-with? (name op) ".")))
            (compile* cenv form)

            (or (symbol? op) (string? op))
            (let [op' (name op)
                  args' (for [x args]
                          (if (symbol? x)
                            (if (lookup cenv x)
                              x
                              (str x))
                            (compile (as-expr cenv) x)))]
              (wrap-with-coerce-fn cenv `(emsh.core/sh ~op' ~@args')))

            :else `(~op ~@(map (partial compile (as-expr cenv)) args)))
      (meta form))))

(defn- compile-coll [cenv form]
  (cond (seq? form) (compile-seq cenv form)
        (map? form) (let [cenv' (as-expr cenv)]
                      (into (empty form)
                            (map (fn [[k v]]
                                   [(compile cenv' k)
                                    (compile cenv' v)]))
                            form))
        :else
        (into (empty form)
              (map (partial compile (as-expr cenv)))
              form)))

(defn compile [cenv form]
  (if (coll? form)
    (compile-coll cenv form)
    form))

(defmethod compile* :default [cenv [op & args :as form]]
  (let [cenv' (as-expr cenv)]
    `(~op ~@(map (partial compile cenv') args))))

(defmethod compile* 'quote [cenv form]
  form)

(defn- compile-body [cenv body]
  (let [cenv' (as-statement cenv)]
    `(~@(map (partial compile cenv') (butlast body))
      ~(compile cenv (last body)))))

(defmethod compile* 'do [cenv [_ & body]]
  `(do ~@(compile-body cenv body)))

(defmethod compile* 'def [cenv [_ name init :as form]]
  (if (= (count form) 2)
    form
    (let [cenv' (-> (as-expr cenv)
                    (assoc-in [:locals name] name))]
      `(def ~name ~(compile cenv' init)))))

(defmethod compile* 'var [cenv form]
  form)

(defmethod compile* 'set! [cenv [_ target expr]]
  `(set! ~target (compile (as-expr cenv) expr)))

(defmethod compile* 'if [cenv [_ test then else :as form]]
  `(if ~(compile (assoc cenv :context :conditional) test)
     ~(compile cenv then)
     ~@(when (= (count form) 4)
         [(compile cenv else)])))

(defn- compile-bindings [cenv bindings]
  (loop [cenv (as-expr cenv)
         [[name init :as binding] & more] (partition 2 bindings)
         ret []]
    (if binding
      (recur (assoc-in cenv [:locals name] name)
             more
             (conj ret name (compile cenv init)))
      [cenv ret])))

(defmethod compile* 'let* [cenv [_ bindings & body]]
  (let [[cenv' bindings'] (compile-bindings cenv bindings)]
    `(let* ~bindings'
       ~@(compile-body (inherit-context cenv' cenv) body))))

(defmethod compile* 'loop* [cenv [_ bindings & body]]
  (let [[cenv' bindings'] (compile-bindings cenv bindings)]
    `(loop* ~bindings'
       ~@(compile-body (inherit-context cenv' cenv) body))))

(defmethod compile* 'fn* [cenv [op & sigs]]
  (let [[fname sigs] (if (symbol? (first sigs))
                       [(first sigs)
                        (cond-> (rest sigs)
                          (vector? (second sigs))
                          list)]
                       [nil
                        (cond-> sigs
                          (vector? (first sigs))
                          list)])
        cenv (as-statement cenv)]
    `(~op ~@(when fname [fname])
          ~@(for [[params & fbody :as sig] sigs
                  :let [names (cons fname params)
                        cenv' (update cenv :locals merge
                                      (zipmap names names))]]
              (with-meta
                `(~params ~@(compile-body cenv' fbody))
                (meta sig))))))

(defmethod compile* 'letfn* [cenv [_ fnspecs & body]]
  (let [fnspecs (partition 2 fnspecs)
        fnames (map first fnspecs)
        cenv' (update cenv :locals merge (zipmap fnames fnames))
        fnspecs' (into []
                       (mapcat (fn [[name f]] [name (compile cenv' f)]))
                       fnspecs)]
    `(letfn* ~fnspecs' ~@(compile-body cenv' body))))

(defmethod compile* 'try [cenv [_ & body]]
  (let [[body' clauses] (split-with
                         (fn [x]
                           (or (not (seq? x))
                               (not ('#{catch finally} (first x)))))
                         body)]
    `(try
       ~@(compile-body cenv body')
       ~@(map (partial compile cenv) clauses))))

(defmethod compile* 'catch [cenv [_ etype ename & body]]
  `(catch ~etype ~ename
     ~@(compile-body (assoc-in cenv [:locals ename] ename) body)))

(defmethod compile* 'finally [cenv [_ & body]]
  `(finally ~@(compile-body (as-statement cenv) body)))
