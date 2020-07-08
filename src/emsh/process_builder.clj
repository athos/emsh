(ns emsh.process-builder
  (:require [clojure.java.io :as io]
            emsh.command
            [emsh.process :as proc]
            [emsh.protocols :as proto])
  (:import [emsh.command Command Pipe Transform]
           [java.io Closeable File OutputStream PipedInputStream
            PipedOutputStream StringReader]
           [java.lang ProcessBuilder$Redirect ProcessBuilder$Redirect$Type]
           [java.util List]))

(extend-protocol proto/IProcessBuilder
  Command
  (start [{:keys [stdin stdout stderr] :as this}]
    (let [pb (ProcessBuilder.
              ^List (cons (:command this) (:args this)))
          env (.environment pb)]
      (doseq [[k v] env]
        (.put env k (str v)))
      (cond (instance? File stdin)
            (.redirectInput pb ^File stdin)

            (nil? stdin)
            (.redirectInput pb ProcessBuilder$Redirect/INHERIT))
      (when (and stdout (not= stdout :pipe))
        (.redirectOutput pb ^ProcessBuilder$Redirect stdout))
      (when stderr
        (.redirectError pb ^ProcessBuilder$Redirect stderr))
      (let [p (proc/->ProcessProxy (.start pb))]
        (when (string? stdin)
          (doto ^OutputStream (proto/output-stream p)
            (spit stdin)
            (.close)))
        (when (nil? stderr)
          (future (io/copy (proto/error-stream p) *err*)))
        (cond-> p
          stdout (assoc :out stdout)))))

  Pipe
  (start [{:keys [commands]}]
    (let [ps (mapv proto/start commands)]
      (doseq [[p q] (partition 2 1 ps)]
        (future
          (let [out (proto/output-stream q)]
            (io/copy (proto/input-stream p) out)
            (.close ^Closeable out))))
      (last ps)))

  Transform
  (start [{:keys [stdin stdout xform]}]
    (let [in (cond (instance? File stdin) (io/reader stdin)
                   (string? stdin) (StringReader. stdin)
                   :else (PipedInputStream.))
          out (if (instance? ProcessBuilder$Redirect stdout)
                (let [^ProcessBuilder$Redirect stdout' stdout
                      file (.file stdout')]
                  (if (= (.type stdout')
                         ProcessBuilder$Redirect$Type/APPEND)
                    (io/writer file :append true)
                    (io/writer file)))
                (PipedOutputStream.))
          sep (System/lineSeparator)
          fut (future
                (with-open [r (io/reader in)
                            w (io/writer out)]
                  (->> (line-seq r)
                       (transduce xform
                                  (completing
                                   (fn [acc ^String line]
                                     (.write w line)
                                     (.write w sep)
                                     acc))
                                  nil))))]
      (proc/->Transform fut
                        (when (instance? PipedOutputStream out)
                          (PipedInputStream. out))
                        (when (instance? PipedInputStream in)
                          (PipedOutputStream. in))))))
