(defproject net.eraserhead/select-nrepl "0.1.0-SNAPSHOT"
  :description "Text object support as nREPL middleware"
  :url "http://github.com/eraserhd/select-nrepl"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [rewrite-clj "0.6.1"]]
  :repl-options {:init-ns dev}
  :plugins [[lein-midje "3.2.1"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.11"]
                                  [midje "1.9.4"]]}})
