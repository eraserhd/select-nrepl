(ns select-nrepl.dev
  (:require
   [clojure.test]
   [clojure.tools.namespace.repl :refer [refresh]]))

(defn run-tests
  "Run tests."
  []
  (clojure.test/run-tests 'select-nrepl.core-test))
