(ns select-nrepl.dev
  (:require
   [clojure.repl :refer :all]
   [clojure.test]
   [clojure.tools.namespace.repl :refer [refresh]]
   [rewrite-clj.zip :as z]))

(defn run-tests
  "Run tests."
  []
  (clojure.test/run-tests 'select-nrepl.core-test))
