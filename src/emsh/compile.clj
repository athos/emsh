(ns emsh.compile
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]
            [emsh.utils :as utils]))

(defn- as-expr [cenv] (assoc cenv :context :expr))
(defn- as-statement [cenv] (assoc cenv :context :statement))

(defn- inherit-context [cenv' cenv]
  (assoc cenv' :context (:context cenv)))

(defmulti ^:private compile* (fn [cenv form] (first form)))

(declare compile)

(defn- compile-seq [{:keys [locals] :as cenv} [op & args :as form]]
  (if-let [v (and (symbol? op) (utils/lookup locals op))]
    (or (when (var? v)
          (cond (and (:macro (meta v))
                     (not= (ns-name (:ns (meta v))) 'emsh.core))
                (compile cenv (apply v form locals args))

                (= (ns-name (:ns (meta v))) 'emsh.core)
                (let [cenv' (assoc cenv :context :shell)]
                  (with-meta
                    `(~(symbol v) ~@(map (partial compile cenv') args))
                    (meta form)))))
        (with-meta
          `(~op ~@(map (partial compile (as-expr cenv)) args))
          (meta form)))
    (with-meta
      (if (and (symbol? op)
               (or (special-symbol? op)
                   (str/starts-with? (name op) ".")
                   (str/ends-with? (name op) ".")))
        (compile* cenv form)
        (let [args' (for [x args]
                      (if (seq? x)
                        (compile (as-expr cenv) x)
                        x))]
          (if-let [coerce-fn (get {:expr `emsh.core/->str*
                                   :statement `emsh.core/->out
                                   :conditional `emsh.core/succeeded?}
                                  (:context cenv))]
            `(~coerce-fn (emsh.core/sh ~op ~@args'))
            `(emsh.core/sh ~op ~@args'))))
      (meta form))))

(defn- compile-coll [cenv form]
  (cond (seq? form) (compile-seq cenv form)
        (map? form) (let [cenv' (as-expr cenv)]
                      (into (empty form)
                            (map (fn [k v]
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
    ;; TODO: should treat recursive calls correctly
    `(def ~name ~(compile (as-expr cenv) init))))

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

(defmethod compile* 'fn* [cenv [_ & sigs]]
  (let [[fname sigs] (if (symbol? (first sigs))
                       [(first sigs) (rest sigs)]
                       [nil sigs])
        cenv (as-statement cenv)]
    `(fn* ~@(when fname [fname])
          ~@(for [[params & fbody :as sig] sigs
                  :let [cenv' (update cenv :locals merge
                                      (zipmap params params))]]
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
