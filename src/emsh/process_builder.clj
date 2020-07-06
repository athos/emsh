(ns emsh.process-builder
  (:require [clojure.java.io :as io]
            [emsh.command :as comm]
            [emsh.protocols :as proto])
  (:import [emsh.command Command Pipe]
           [java.lang ProcessBuilder$Redirect]
           [java.util List]))

(extend-protocol proto/IProcessBuilder
  Command
  (start [{:keys [stdin stdout stderr] :as this}]
    (let [pb (ProcessBuilder.
              ^List (cons (:command this) (:args this)))
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
        (cond-> (comm/->ProcessProxy p)
          stdout (assoc :out stdout)))))

  Pipe
  (start [{:keys [commands]}]
    (let [ps (mapv proto/start commands)]
      (doseq [[p q] (partition 2 1 ps)]
        (future
          (let [out (.getOutputStream (comm/process-impl q))]
            (io/copy (.getInputStream (comm/process-impl p)) out)
            (.close out))))
      (last ps))))
