(ns emsh.core
  (:refer-clojure :exclude [< >])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [emsh.compile :as comp])
  (:import [java.io OutputStream]))

(defn- ^Process ensure-started [p]
  (if (instance? ProcessBuilder p)
    (.start ^ProcessBuilder p)
    p))

(defn ->str [x]
  (if (instance? ProcessBuilder x)
    (slurp (.getInputStream (ensure-started x)))
    (str x)))

(defn ->str* [p]
  (let [sb (StringBuilder.)]
    (with-open [r (-> (ensure-started p)
                      .getInputStream
                      io/reader)]
      (loop []
        (when-let [line (.readLine r)]
          (.append sb line)
          (recur))))
    (.toString sb)))

(defn ->lines [p]
  (line-seq (io/reader (.getInputStream (ensure-started p)))))

(defn ->out
  ([p] (->out p *out*))
  ([p out]
   (io/copy (.getInputStream (ensure-started p)) out)))

(defn wait-for [p]
  (.waitFor (ensure-started p)))

(defn succeeded? [p]
  (= (wait-for p) 0))

(defn sh [command & args]
  (->> (cons command args)
       (map ->str)
       (ProcessBuilder.)))

(defn copy [in ^OutputStream out]
  (io/copy in out)
  (.close out))

(defn | [& ps]
  (let [ps' (mapv ensure-started ps)]
    (doseq [[p q] (partition 2 1 ps')]
      (future
        (copy (.getInputStream ^Process p)
              (.getOutputStream ^Process q))))
    (last ps')))

(defn < [^ProcessBuilder p in]
  (.redirectInput p (io/file in)))

(defn > [^ProcessBuilder p out]
  (.redirectOutput p (io/file out)))

(defn >> [^ProcessBuilder p out]
  (.redirectOutput p (java.lang.ProcessBuilder$Redirect/appendTo (io/file out))))

(defmacro do [& body]
  (with-meta
    (comp/compile {:locals &env :context :statement} `(do ~@body))
    (meta &form)))
