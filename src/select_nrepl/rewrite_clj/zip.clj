(ns select-nrepl.rewrite-clj.zip
  (:refer-clojure :exclude [next find replace remove
                            seq? map? vector? list? set?
                            print map get assoc])
  (:require [select-nrepl.rewrite-clj.zip
             [base :as base]
             [edit :as edit]
             [find :as find]
             [insert :as insert]
             [move :as move]
             [remove :as remove]
             [seq :as seq]
             [subedit :as subedit]
             [walk :as walk]
             [whitespace :as ws]]
            [select-nrepl.rewrite-clj.potemkin :refer [import-vars]]
            [select-nrepl.rewrite-clj
             [parser :as p]
             [node :as node]]
            [select-nrepl.rewrite-clj.custom-zipper.core :as z]))

;; ## API Facade

(import-vars
  [select-nrepl.rewrite-clj.custom-zipper.core
   node position root]

  [select-nrepl.rewrite-clj.zip.base
   child-sexprs
   edn* edn tag sexpr
   length value
   of-file of-string
   string root-string
   print print-root]

  [select-nrepl.rewrite-clj.zip.edit
   replace edit splice
   prefix suffix]

  [select-nrepl.rewrite-clj.zip.find
   find find-next
   find-depth-first
   find-next-depth-first
   find-tag find-next-tag
   find-value find-next-value
   find-token find-next-token]

  [select-nrepl.rewrite-clj.zip.insert
   insert-right insert-left
   insert-child append-child]

  [select-nrepl.rewrite-clj.zip.move
   left right up down prev next
   leftmost rightmost
   leftmost? rightmost? end?]

  [select-nrepl.rewrite-clj.zip.remove
   remove]

  [select-nrepl.rewrite-clj.zip.seq
   seq? list? vector? set? map?
   map map-keys map-vals
   get assoc]

  [select-nrepl.rewrite-clj.zip.subedit
   edit-node edit-> edit->>
   subedit-node subedit-> subedit->>]

  [select-nrepl.rewrite-clj.zip.walk
   prewalk
   postwalk]

  [select-nrepl.rewrite-clj.zip.whitespace
   whitespace? linebreak?
   whitespace-or-comment?
   skip skip-whitespace
   skip-whitespace-left
   prepend-space append-space
   prepend-newline append-newline])

;; ## Base Operations

(defmacro ^:private defbase
  [sym base]
  (let [{:keys [arglists]} (meta
                             (ns-resolve
                               (symbol (namespace base))
                               (symbol (name base))))
        sym (with-meta
              sym
              {:doc (format "Directly call '%s' on the given arguments." base)
               :arglists `(quote ~arglists)})]
    `(def ~sym ~base)))

(defbase right* select-nrepl.rewrite-clj.custom-zipper.core/right)
(defbase left* select-nrepl.rewrite-clj.custom-zipper.core/left)
(defbase up* select-nrepl.rewrite-clj.custom-zipper.core/up)
(defbase down* select-nrepl.rewrite-clj.custom-zipper.core/down)
(defbase next* select-nrepl.rewrite-clj.custom-zipper.core/next)
(defbase prev* select-nrepl.rewrite-clj.custom-zipper.core/prev)
(defbase rightmost* select-nrepl.rewrite-clj.custom-zipper.core/rightmost)
(defbase leftmost* select-nrepl.rewrite-clj.custom-zipper.core/leftmost)
(defbase replace* select-nrepl.rewrite-clj.custom-zipper.core/replace)
(defbase edit* select-nrepl.rewrite-clj.custom-zipper.core/edit)
(defbase remove* select-nrepl.rewrite-clj.custom-zipper.core/remove)
(defbase insert-left* select-nrepl.rewrite-clj.custom-zipper.core/insert-left)
(defbase insert-right* select-nrepl.rewrite-clj.custom-zipper.core/insert-right)

;; ## DEPRECATED

(defn ^{:deprecated "0.4.0"} ->string
  "DEPRECATED. Use `string` instead."
  [zloc]
  (string zloc))

(defn ^{:deprecated "0.4.0"} ->root-string
  "DEPRECATED. Use `root-string` instead."
  [zloc]
  (root-string zloc))
