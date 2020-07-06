(ns emsh.process
  (:require [emsh.protocols :as proto]))

(defrecord ProcessProxy [^Process process]
  proto/IProcess
  (input-stream [{:keys [out]}]
    (when (or (nil? out) (= out :pipe))
      (.getInputStream process)))
  (output-stream [this]
    (.getOutputStream process))
  (error-stream [this]
    (.getErrorStream process))
  (wait-for [this]
    (.waitFor process)))

(defrecord Transform [fut in out]
  proto/IProcess
  (input-stream [this] in)
  (output-stream [this] out)
  (error-stream [this])
  (wait-for [this]
    (try
      @fut
      0
      (catch Throwable _ 1))))
