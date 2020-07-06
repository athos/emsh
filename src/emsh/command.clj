(ns emsh.command
  (:refer-clojure :exclude [< >])
  (:require [clojure.java.io :as io])
  (:import [java.lang ProcessBuilder$Redirect]
           [java.util List]))

(defrecord ProcessProxy [process])

(defn ^Process process-impl [p]
  (:process p))

(defprotocol ICommand
  (start [this])
  (list-commands [this])
  (< [this input])
  (> [this from to]))

(defrecord Command [command args env]
  ICommand
  (start [{:keys [stdin stdout stderr]}]
    (let [pb (ProcessBuilder. ^List (cons command args))
          env (.environment pb)]
      (doseq [[k v] env]
        (.put env k (str v)))
      (when (nil? stdin)
        (.redirectInput pb ProcessBuilder$Redirect/INHERIT))
      (when (and stdout (not= stdout :pipe))
        (.redirectOutput pb ^ProcessBuilder$Redirect stdout))
      (when stderr
        (.redirectError pb ^ProcessBuilder$Redirect stderr))
      (let [p (.start pb)]
        (when (nil? stderr)
          (future (io/copy (.getErrorStream p) *err*)))
        (cond-> (->ProcessProxy p)
          stdout (assoc :out stdout)))))
  (list-commands [this] [this])
  (< [this input]
    (assoc this :stdin input))
  (> [this from to]
    (assoc this from to)))

(defrecord Pipe [commands]
  ICommand
  (start [this]
    (let [ps (mapv start commands)]
      (doseq [[p q] (partition 2 1 ps)]
        (future
          (let [out (.getOutputStream (process-impl q))]
            (io/copy (.getInputStream (process-impl p)) out)
            (.close out))))
      (last ps)))
  (list-commands [this] commands)
  (< [this input]
    (update-in this [:commands 0] < input))
  (> [this from to]
    (update-in this [:commands (dec (count commands))] > from to)))
