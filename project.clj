(defproject lisp-interpreter "0.1.0-SNAPSHOT"
  :description "TLC Lisp interpreter written in Clojure"
  :url ""
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :repl-options {:init-ns lisp-interpreter.main}
  :main lisp-interpreter.main/main)
