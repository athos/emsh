(ns emsh.repl
  (:require [clojure.main :as main]
            [emsh.core :as em]))

(defn wrap-read-fn [read-fn]
  (fn [prompt exit]
    (let [res (read-fn prompt exit)]
      (if (or (= res prompt) (= res exit))
        res
        `(em/do ~res)))))

(defn repl [& options]
  (let [opts (-> (apply array-map options)
                 (update :read (fnil wrap-read-fn main/repl-read)))]
    (apply main/repl (into [] cat opts))))

(defn -main []
  (repl))
