(ns emsh.command
  (:require [clojure.java.io :as io]
            [emsh.protocols :as proto]))

(defrecord Command [command args env]
  proto/ICommand
  (list-commands [this] [this])
  (< [this input]
    (assoc this :stdin input))
  (> [this from to]
    (assoc this from to)))

(defrecord Pipe [commands]
  proto/ICommand
  (list-commands [this] commands)
  (< [this input]
    (update-in this [:commands 0] proto/< input))
  (> [this from to]
    (update-in this [:commands (dec (count commands))] proto/> from to)))

(defrecord Transform [xform]
  proto/ICommand
  (list-commands [this] [this])
  (< [this input]
    (assoc this :stdin input))
  (> [this from to]
    (assoc this from to)))
