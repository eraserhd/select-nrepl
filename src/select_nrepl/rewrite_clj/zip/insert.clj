(ns ^:no-doc select-nrepl.rewrite-clj.zip.insert
  (:require [select-nrepl.rewrite-clj.zip
             [base :as base]
             [whitespace :as ws]]
            [select-nrepl.rewrite-clj.node :as node]
            [select-nrepl.rewrite-clj.custom-zipper.core :as z]))

(def ^:private space
  (node/spaces 1))

(defn- insert
  "Generic insertion helper. If the node reached by `move-fn`
   is a whitespace, insert an additional space."
  [move-fn insert-fn prefix zloc item]
  (let [item-node (node/coerce item)
        next-node (move-fn zloc)]
    (->> (concat
           (if (and next-node (not (ws/whitespace? next-node)))
             [space])
           [item-node]
           (if (not (ws/whitespace? zloc))
             prefix)) 
         (reduce insert-fn zloc))))

(defn insert-right
  "Insert item to the right of the current location. Will insert a space if necessary."
  [zloc item]
  (insert
    z/right
    z/insert-right
    [space]
    zloc item))

(defn insert-left
  "Insert item to the right of the left location. Will insert a space if necessary."
  [zloc item]
  (insert
    z/left
    z/insert-left
    [space]
    zloc item))

(defn insert-child
  "Insert item as first child of the current node. Will insert a space if necessary."
  [zloc item]
  (insert
    z/down
    z/insert-child
    []
    zloc item))

(defn append-child
  "Insert item as last child of the current node. Will insert a space if necessary."
  [zloc item]
  (insert
    #(some-> % z/down z/rightmost)
    z/append-child
    []
    zloc item))
