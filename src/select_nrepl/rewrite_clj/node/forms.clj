(ns ^:no-doc select-nrepl.rewrite-clj.node.forms
  (:require [select-nrepl.rewrite-clj.node.protocols :as node]))

;; ## Node

(defrecord FormsNode [children]
  node/Node
  (tag [_]
    :forms)
  (printable-only? [_]
    false)
  (sexpr [_]
    (let [es (node/sexprs children)]
      (if (next es)
        (list* 'do es)
        (first es))))
  (length [_]
    (node/sum-lengths children))
  (string [_]
    (node/concat-strings children))

  node/InnerNode
  (inner? [_]
    true)
  (children [_]
    children)
  (replace-children [this children']
    (assoc this :children children'))
  (leader-length [_]
    0)

  Object
  (toString [this]
    (node/string this)))

(node/make-printable! FormsNode)

;; ## Constructor

(defn forms-node
  "Create top-level node wrapping multiple children
   (equals an implicit `do` on the top-level)."
  [children]
  (->FormsNode children))
