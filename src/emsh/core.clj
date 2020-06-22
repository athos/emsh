(ns emsh.core
  (:refer-clojure :exclude [< >])
  (:require [clojure.java.io :as io]
            [emsh.compile :as comp])
  (:import [java.io OutputStream BufferedReader]
           [java.lang ProcessBuilder$Redirect]))

(defrecord ProcessProxy [process])

(defn- ^Process process-impl [process-proxy]
  (:process process-proxy))

(def ^:dynamic *env* {})

(defn- start-process [^ProcessBuilder pb]
  (let [out (.redirectOutput pb)
        err (.redirectError pb)
        env (.environment pb)]
    (doseq [[k v] *env*]
      (.put env k (str v)))
    (let [p (.start pb)]
      (when-not (= err ProcessBuilder$Redirect/PIPE)
        (future
          (io/copy (.getErrorStream p) *err*)))
      (cond-> (->ProcessProxy p)
        (not= out ProcessBuilder$Redirect/PIPE)
        (assoc :out out)))))

(defmacro with-env [env & body]
  `(binding [*env* (merge *env*
                          ~(into {} (map (fn [[k v]] [(name k) v]))
                                 (partition 2 env)))]
     ~@body))

(defn- ensure-started [p]
  (if (instance? ProcessBuilder p)
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

(defn ^:process-out sh [command & args]
  (->> (cons command args)
       ^java.util.List (map str)
       (ProcessBuilder.)))

(defn ^:process-in ^:process-out | [& ps]
  (let [ps' (mapv ensure-started ps)]
    (doseq [[p q] (partition 2 1 ps')]
      (future
        (let [out (.getOutputStream (process-impl q))]
          (io/copy (.getInputStream (process-impl p)) out)
          (.close out))))
    (last ps')))

(defn ^:process-in ^:process-out < [^ProcessBuilder p in]
  (.redirectInput p (io/file in)))

(defn ^:process-in ^:process-out > [^ProcessBuilder p out]
  (.redirectOutput p (io/file out)))

(defn ^:process-in ^:process-out >> [^ProcessBuilder p out]
  (.redirectOutput p (java.lang.ProcessBuilder$Redirect/appendTo (io/file out))))

(defmacro do [& body]
  (with-meta
    (comp/compile {:locals &env :context :statement} `(do ~@body))
    (meta &form)))
