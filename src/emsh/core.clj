(ns emsh.core
  (:refer-clojure :exclude [< > macroexpand])
  (:require [clojure.core :as cc]
            [clojure.java.io :as io]
            [emsh.compile :as comp])
  (:import [java.io OutputStream BufferedReader]
           [java.lang ProcessBuilder$Redirect]))

(defrecord Command [command args])

(defrecord ProcessProxy [process])

(defn- ^Process process-impl [process-proxy]
  (:process process-proxy))

(def ^:dynamic *env* {})

(def ^:dynamic *in-pipe?* false)

(defn- start-process [{:keys [in out err] :as cmd}]
  (let [pb (ProcessBuilder. ^java.util.List (cons (:command cmd) (:args cmd)))
        env (.environment pb)]
    (doseq [[k v] *env*]
      (.put env k (str v)))
    (when (and (not *in-pipe?*) (nil? in))
      (.redirectInput pb ProcessBuilder$Redirect/INHERIT))
    (when out
      (.redirectOutput pb ^ProcessBuilder$Redirect out))
    (when err
      (.redirectError pb ^ProcessBuilder$Redirect err))
    (let [p (.start pb)]
      (when (nil? err)
        (future (io/copy (.getErrorStream p) *err*)))
      (cond-> (->ProcessProxy p)
        out (assoc :out out)))))

(defmacro with-env [env & body]
  `(binding [*env* (merge *env*
                          ~(into {} (map (fn [[k v]] [(name k) v]))
                                 (partition 2 env)))]
     ~@body))

(defn- ensure-started [p]
  (if (instance? Command p)
    (start-process p)
    p))

(defn- input-stream [p]
  (when (nil? (:out p))
    (.getInputStream (process-impl p))))

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
    (.waitFor (process-impl p'))))

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
  (->> (flatten args)
       (map str)
       (->Command command)))

(defn ^:process-in ^:process-out | [& ps]
  (let [ps' (into [(ensure-started (first ps))]
                  (map (fn [p]
                         (binding [*in-pipe?* true]
                           (ensure-started p))))
                  (rest ps))]
    (doseq [[p q] (partition 2 1 ps')]
      (future
        (let [out (.getOutputStream (process-impl q))]
          (io/copy (.getInputStream (process-impl p)) out)
          (.close out))))
    (last ps')))

(defn ^:process-in ^:process-out < [cmd in]
  (assoc cmd :in in))

(defn ^:process-in ^:process-out >
  ([cmd dst] (> cmd :stdout dst))
  ([cmd src dst]
   (let [redirect (ProcessBuilder$Redirect/to (io/file dst))]
     (condp contains? src
       #{1 :stdout} (assoc cmd :out redirect)
       #{2 :stderr} (assoc cmd :err redirect)))))

(defn ^:process-in ^:process-out >>
  ([cmd dst] (>> cmd :stdout dst))
  ([cmd src dst]
   (let [redirect (ProcessBuilder$Redirect/appendTo (io/file dst))]
     (condp contains? src
       #{1 :stdout} (assoc cmd :out redirect)
       #{2 :stderr} (assoc cmd :err redirect)))))

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
