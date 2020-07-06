(ns emsh.protocols
  (:refer-clojure :exclude [< >]))

(defprotocol ICommand
  (list-commands [this])
  (< [this input])
  (> [this from to]))

(defprotocol IProcessBuilder
  (start [this]))
