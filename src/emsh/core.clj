(ns emsh.core
  (:refer-clojure :exclude [< >])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [emsh.compile :as comp]
            [emsh.utils :as utils])
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

(defn exit-value [^Process p]
  (.exitValue p))

(defn succeeded? [^Process p]
  (.waitFor p)
  (= (exit-value p) 0))

(defn sh* [command & args]
  (let [builder (->> (cons command args)
                     (map ->str)
                     (ProcessBuilder.))]
    (.start builder)))

(defmacro sh [command & args]
  `(sh* ~(str command)
        ~@(map (fn [x]
                 (if (and (symbol? x) (not (utils/lookup &env x)))
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

(defmacro do [& body]
  (with-meta
    (comp/compile {:locals &env :context :statement} `(do ~@body))
    (meta &form)))
