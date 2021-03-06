(ns ^:no-doc select-nrepl.rewrite-clj.parser.whitespace
  (:require [select-nrepl.rewrite-clj
             [node :as node]
             [reader :as reader]]))

(defn parse-whitespace
  "Parse as much whitespace as possible. The created node can either contain
   only linebreaks or only space/tabs."
  [reader]
  (let [c (reader/peek reader)]
    (cond (reader/linebreak? c)
          (node/newline-node
            (reader/read-while reader reader/linebreak?))

          (reader/comma? c)
          (node/comma-node
            (reader/read-while reader reader/comma?))

          :else
          (node/whitespace-node
            (reader/read-while reader reader/space?)))))
