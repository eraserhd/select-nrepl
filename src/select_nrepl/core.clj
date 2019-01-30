(ns select-nrepl.core
  (:require
   [rewrite-clj.zip :as z]))

(defmulti select
  (fn [kind text start end]
    kind))

(defn- ->zero-based [[i j]]
  [(dec i) (dec j)])

(defn- start-position [z]
  (->zero-based (z/position z)))

(defn- end-position [z]
  (let [[i j] (start-position z)
        s (z/string z)
        lines (count (filter #{\newline} (seq s)))
        columns (count (re-find #"[^\n]*$" s))]
    [(+ i lines)
     (if (zero? lines)
       (+ j columns)
       columns)]))

(defn- position<=? [a b]
  (<= (compare a b) 0))

(defn- inside?
  "Is i.j inside the node pointed to by z?"
  [p z]
  (when z
    (and (position<=? (start-position z) p)
         (position<=? p (end-position z)))))

(defn- this-node
  [z]
  (when z
    [(start-position z) (end-position z)]))

(defmethod select :element
  [_ text start _]
  (-> (z/of-string text {:track-position? true})
      (z/find-depth-first (partial inside? start))
      this-node))
