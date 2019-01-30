(ns select-nrepl.core-test
  (:require
   [clojure.test :refer :all]
   [select-nrepl.core]))

(defn- select [kind text]
  text)

(deftest a-select-element
  (are [before after] (= (select :element before) after)
    "<hello>" "<hello>"))
