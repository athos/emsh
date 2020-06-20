(ns emsh.core
  (:refer-clojure :exclude [< >])
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io OutputStream]))

(defn ->str [x]
  (if (instance? Process x)
    (slurp (.getInputStream ^Process x))
    (str x)))

(defn ->str* [^Process p]
  (let [sb (StringBuilder.)]
    (with-open [r (io/reader (.getInputStream p))]
      (loop []
        (when-let [line (.readLine r)]
          (.append sb line)
          (recur))))
    (.toString sb)))

(defn ->strs [^Process p]
  (line-seq (io/reader (.getInputStream p))))

(defn ->out
  ([p] (->out p *out*))
  ([^Process p out]
   (io/copy (.getInputStream p) out)))

(defn sh* [command & args]
  (let [builder (->> (cons command args)
                     (map ->str)
                     (ProcessBuilder.))]
    (.start builder)))

(defn lookup [locals sym]
  (or (get locals sym)
      (resolve '{do do} sym)
      (when (and (nil? (namespace sym))
                 (not= sym 'do))
        (resolve (symbol "emsh.core" (name sym))))))

(defmacro sh [command & args]
  `(sh* ~(str command)
        ~@(map (fn [x]
                 (if (and (symbol? x) (not (lookup &env x)))
                   (str x)
                   x))
               args)))

(defn copy [in ^OutputStream out]
  (io/copy in out)
  (.close out))

(defn | [& ps]
  (doseq [[p q] (partition 2 1 ps)]
    (future
      (copy (.getInputStream ^Process p)
            (.getOutputStream ^Process q))))
  (last ps))

(defn < [^Process p in]
  (future (copy (io/input-stream in) (.getOutputStream p)))
  p)

(defn > [^Process p out]
  (future (copy (.getInputStream p) (io/output-stream out)))
  p)

(defn >> [^Process p out]
  (future (copy (.getInputStream p) (io/output-stream out :append true)))
  p)

(defn as-expr [cenv] (assoc cenv :context :expr))
(defn as-statement [cenv] (assoc cenv :context :statement))

(defn inherit-context [cenv' cenv]
  (assoc cenv' :context (:context cenv)))

(defmulti convert* (fn [cenv form] (first form)))

(declare convert)

(defn convert-seq [{:keys [locals] :as cenv} [op & args :as form]]
  (if-let [v (and (symbol? op) (lookup locals op))]
    (or (when (var? v)
          (cond (and (:macro (meta v))
                     (not= (ns-name (:ns (meta v))) 'emsh.core))
                (convert cenv (apply v form locals args))

                (= (ns-name (:ns (meta v))) 'emsh.core)
                (let [cenv' (assoc cenv :context :shell)]
                  (with-meta
                    `(~(symbol v) ~@(map (partial convert cenv') args))
                    (meta form)))))
        (with-meta
          `(~op ~@(map (partial convert (as-expr cenv)) args))
          (meta form)))
    (with-meta
      (if (and (symbol? op)
               (or (special-symbol? op)
                   (str/starts-with? (name op) ".")
                   (str/ends-with? (name op) ".")))
        (convert* cenv form)
        (let [args' (for [x args]
                      (if (seq? x)
                        (convert (as-expr cenv) x)
                        x))]
          (if-let [coerce-fn (get {:expr `->str*, :statement `->out}
                                  (:context cenv))]
            `(~coerce-fn (sh ~op ~@args'))
            `(sh ~op ~@args'))))
      (meta form))))

(defn convert-coll [cenv form]
  (cond (seq? form) (convert-seq cenv form)
        (map? form) (let [cenv' (as-expr cenv)]
                      (into (empty form)
                            (map (fn [k v]
                                   [(convert cenv' k)
                                    (convert cenv' v)]))
                            form))
        :else
        (into (empty form)
              (map (partial convert (as-expr cenv)))
              form)))

(defn convert [cenv form]
  (if (coll? form)
    (convert-coll cenv form)
    form))

(defmethod convert* :default [cenv [op & args :as form]]
  (let [cenv' (as-expr cenv)]
    `(~op ~@(map (partial convert cenv') args))))

(defn convert-body [cenv body]
  (let [cenv' (as-statement cenv)]
    `(~@(map (partial convert cenv') (butlast body))
      ~(convert cenv (last body)))))

(defmethod convert* 'do [cenv [_ & body]]
  `(do ~@(convert-body cenv body)))

(defmethod convert* 'def [cenv [_ name init :as form]]
  (if (= (count form) 2)
    form
    ;; TODO: should treat recursive calls correctly
    `(def ~name ~(convert (as-expr cenv) init))))

(defmethod convert* 'if [cenv [_ test then else :as form]]
  `(if ~(convert (as-expr cenv) test)
     ~(convert cenv then)
     ~@(when (= (count form) 4)
         [(convert cenv else)])))

(defn convert-bindings [cenv bindings]
  (loop [cenv (as-expr cenv)
         [[name init :as binding] & more] (partition 2 bindings)
         ret []]
    (if binding
      (recur (assoc-in cenv [:locals name] name)
             more
             (conj ret name (convert cenv init)))
      [cenv ret])))

(defmethod convert* 'let* [cenv [_ bindings & body]]
  (let [[cenv' bindings'] (convert-bindings cenv bindings)]
    `(let* ~bindings'
       ~@(convert-body (inherit-context cenv' cenv) body))))

(defmethod convert* 'loop* [cenv [_ bindings & body]]
  (let [[cenv' bindings'] (convert-bindings cenv bindings)]
    `(loop* ~bindings'
       ~@(convert-body (inherit-context cenv' cenv) body))))

(defmethod convert* 'fn* [cenv [_ & sigs]]
  (let [[fname sigs] (if (symbol? (first sigs))
                       [(first sigs) (rest sigs)]
                       [nil sigs])
        cenv (as-statement cenv)]
    `(fn* ~@(when fname [fname])
          ~@(for [[params & fbody :as sig] sigs
                  :let [cenv' (update cenv :locals merge
                                      (zipmap params params))]]
              (with-meta
                `(~params ~@(convert-body cenv' fbody))
                (meta sig))))))

(defmethod convert* 'letfn* [cenv [_ fnspecs & body]]
  (let [fnspecs (partition 2 fnspecs)
        fnames (map first fnspecs)
        cenv' (update cenv :locals merge (zipmap fnames fnames))
        fnspecs' (into []
                       (mapcat (fn [[name f]] [name (convert cenv' f)]))
                       fnspecs)]
    `(letfn* ~fnspecs' ~@(convert-body cenv' body))))

(defmacro do [& body]
  (with-meta
    (convert {:locals &env :context :statement} `(do ~@body))
    (meta &form)))
