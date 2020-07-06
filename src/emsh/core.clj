(ns emsh.core
  (:refer-clojure :exclude [< > macroexpand])
  (:require [clojure.core :as cc]
            [clojure.java.io :as io]
            [emsh.command :as comm]
            [emsh.compile :as comp])
  (:import [java.io BufferedReader]
           [java.lang ProcessBuilder$Redirect]))

(def ^:dynamic *env* {})

(defmacro with-env [env & body]
  `(binding [*env* (merge *env*
                          ~(into {} (map (fn [[k v]] [(name k) v]))
                                 (partition 2 env)))]
     ~@body))

(defn- ensure-started [p]
  (if (satisfies? comm/ICommand p)
    (comm/start p)
    p))

(defn- input-stream [p]
  (when (nil? (:out p))
    (.getInputStream (comm/process-impl p))))

(defn- with-input-stream [p f]
  (when-let [in (input-stream (ensure-started p))]
    (f in)))

(defn ^:process-in ->raw-str [x]
  (with-input-stream x slurp))

(defn ^:process-in ->str [p]
  (with-input-stream p
    (fn [in]
      (let [sb (StringBuilder.)]
        (with-open [r (io/reader in)]
          (loop []
            (when-let [line (.readLine ^BufferedReader r)]
              (.append sb line)
              (recur))))
        (.toString sb)))))

(defn ^:process-in ->lines [p]
  (with-input-stream p #(line-seq (io/reader %))))

(defn ^:process-in ->out
  ([p] (->out p *out*))
  ([p out]
   (with-input-stream p #(io/copy % out))))

(defn ^:process-in exit-value [p]
  (let [p' (ensure-started p)]
    (->out p')
    (.waitFor (comm/process-impl p'))))

(defn ^:process-in wait-for [p]
  (exit-value p)
  nil)

(defn ^:process-in succeeded? [p]
  (= (exit-value p) 0))

(defn ^:process-in ->proc [p]
  (let [p' (ensure-started p)]
    (wait-for p')
    p'))

(defn ^:process-out sh [command & args]
  (comm/->Command command
                  (map str (flatten args))
                  *env*))

(defn ^:process-in ^:process-out | [command & commands]
  (if (seq commands)
    (as-> (cons command commands) <>
      (into [] (mapcat comm/list-commands) <>)
      (concat [(assoc (first <>) :stdout :pipe)]
              (map #(assoc % :stdin :pipe :stdout :pipe)
                   (next (butlast <>)))
              [(assoc (last <>) :stdin :pipe)])
      (comm/->Pipe (vec <>)))
    command))

(defn ^:process-in ^:process-out < [cmd in]
  (comm/< cmd in))

(defn ^:process-in ^:process-out >
  ([cmd to] (> cmd :stdout to))
  ([cmd from to]
   (let [redirect (ProcessBuilder$Redirect/to (io/file to))]
     (condp contains? from
       #{1 :stdout} (comm/> cmd :stdout redirect)
       #{2 :stderr} (comm/> cmd :stderr redirect)))))

(defn ^:process-in ^:process-out >>
  ([cmd to] (>> cmd :stdout to))
  ([cmd from to]
   (let [redirect (ProcessBuilder$Redirect/appendTo (io/file to))]
     (condp contains? from
       #{1 :stdout} (comm/> cmd :stdout redirect)
       #{2 :stderr} (comm/> cmd :stderr redirect)))))

(defn ^:process-out proc [p] p)

(defmacro && [p & ps]
  (if (seq ps)
    `(let [p# (->proc ~p)]
       (if (succeeded? p#)
         (&& ~@ps)
         (proc p#)))
    p))

(defmacro || [p && ps]
  (if (seq ps)
    `(let [p# (->proc ~p)]
       (if (succeeded? p#)
         (proc p#)
         (|| ~@ps)))
    p))

(defmacro do [& body]
  (let [ctx (:context (meta &form) :statement)]
    (with-meta
      (comp/compile {:locals &env :context ctx} `(do ~@body))
      (dissoc (meta &form) :context))))

(defn macroexpand [form]
  (binding [comp/*debug* true]
    (cc/macroexpand form)))
