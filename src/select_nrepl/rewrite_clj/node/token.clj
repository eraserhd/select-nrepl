(ns ^:no-doc select-nrepl.rewrite-clj.node.token
  (:require [select-nrepl.rewrite-clj.node.protocols :as node]))

;; ## Node

(defrecord TokenNode [value string-value]
  node/Node
  (tag [_] :token)
  (printable-only? [_] false)
  (sexpr [_] value)
  (length [_] (count string-value))
  (string [_] string-value)

  Object
  (toString [this]
    (node/string this)))

(node/make-printable! TokenNode)

;; ## Constructor

(defn token-node
  "Create node for an unspecified EDN token."
  [value & [string-value]]
  (->TokenNode
    value
    (or string-value (pr-str value))))
