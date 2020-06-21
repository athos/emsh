(ns emsh.utils)

(defn lookup [locals sym]
  (or (get locals sym)
      (resolve '{do do} sym)
      (when (and (nil? (namespace sym))
                 (not= sym 'do))
        (resolve (symbol "emsh.core" (name sym))))))
