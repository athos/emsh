(defproject emsh "0.1.0-SNAPSHOT"
  :description "An embedded shell built on top of Clojure"
  :url "https://github.com/athos/emsh"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :repl-options {:init-ns emsh.core})
