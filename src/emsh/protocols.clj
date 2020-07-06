(ns emsh.protocols
  (:refer-clojure :exclude [< >]))

(defprotocol ICommand
  (list-commands [this])
  (< [this input])
  (> [this from to]))

(defprotocol IProcess
  (input-stream [this])
  (output-stream [this])
  (error-stream [this])
  (wait-for [this]))

(defprotocol IProcessBuilder
  (start [this]))
