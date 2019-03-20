(defproject net.eraserhead/select-nrepl "0.1.1-SNAPSHOT"
  :description "Text object support as nREPL middleware"
  :url "http://github.com/eraserhd/select-nrepl"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_username
                                    :password :env/clojars_password}]]
  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "v"]
                  ["deploy" "clojars"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]]
  :dependencies []
  :plugins [[lein-midje "3.2.1"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.9.0"]
                                  [org.clojure/tools.namespace "0.2.11"]
                                  [midje "1.9.4"]]
                   :repl-options {:init-ns dev
                                  :nrepl-middleware [select-nrepl.core/wrap-select]}}})
