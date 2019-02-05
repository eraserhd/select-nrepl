(ns select-nrepl.core
  (:require
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [nrepl.misc :refer [response-for]]
   [nrepl.transport :as t]))

(defmulti select
  (fn [kind text start end]
    kind))

(defn- start-position [z]
  (z/position z))

(defn- end-position [z]
  (let [[i j] (start-position z)
        s (z/string z)
        lines (count (filter #{\newline} (seq s)))
        columns (count (re-find #"[^\n]*$" s))]
    [(+ i lines)
     (dec (if (zero? lines)
            (+ j columns)
            columns))]))

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

(defn- element?
  [z]
  (case (z/tag z)
    :reader-macro               (= :token (-> z z/down z/right z/tag))
    (:token :regex :multi-line) true
    false))

(defmethod select :element
  [_ text start _]
  (-> (z/of-string text {:track-position? true})
      (z/find-depth-first (fn [z]
                            (and (inside? start z)
                                 (element? z))))
      this-node))

(defn- response-for-select
  [{:keys [code
           kind
           selection-start-line
           selection-start-column
           selection-end-line
           selection-end-column],
    :or {selection-end-line selection-start-line
         selection-end-column selection-start-column},
    :as message}]
  (let [[start' end'] (select (keyword kind)
                              code
                              [selection-start-line selection-start-column]
                              [selection-end-line selection-end-column])]
    (response-for message
                  :status :done
                  :selection-start-line (first start')
                  :selection-start-column (second start')
                  :selection-end-line (first end')
                  :selection-end-column (second end'))))

(defn wrap-select
  [f]
  (fn [{:keys [op transport] :as message}]
    (if (= "select" op)
      (t/send transport (response-for-select message))
      (f message))))
