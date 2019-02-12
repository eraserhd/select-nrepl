(ns select-nrepl.core
  (:require
   [select-nrepl.rewrite-clj.node :as n]
   [select-nrepl.rewrite-clj.zip :as z]
   [nrepl.middleware :refer [set-descriptor!]]
   [nrepl.misc :refer [response-for]]
   [nrepl.transport :as t]))

(def ^:private start-position z/position)

(defn- end-position [z]
  (let [[i j] (start-position z)
        s (z/string z)
        lines (count (filter #{\newline} (seq s)))
        columns (count (re-find #"[^\n]*$" s))]
    [(+ i lines) (if (zero? lines)
                   (+ j columns -1)
                   columns)]))

(defn- position<=? [a b]
  (<= (compare a b) 0))

(defn- element?
  [z]
  (case (z/tag z)
    :reader-macro               (= :token (-> z z/down z/right z/tag))
    (:token :regex :multi-line) true
    false))

(def ^:private form? (comp #{:list :map :set :vector} z/tag))

(defn- inside?
  [z cursor]
  (and (position<=? (start-position z) cursor)
       (position<=? cursor (end-position z))))

(defn- find-inside [z cursor ok?]
  (loop [z z]
    (cond
      (nil? z)           nil
      (inside? z cursor) (if-let [z' (some-> z z/down (find-inside cursor ok?))]
                           z'
                           (if (ok? z)
                             z
                             (recur (z/right z))))
      :else              (recur (z/right z)))))

(defn- acceptable?
  "A node is acceptable if it contains the cursor or starts after
  cursor.  In other words, if it ends after the cursor (inclusive)."
  [z cursor]
  (position<=? cursor (end-position z)))

(defn- find-acceptable [z cursor ok?]
  (loop [z z]
    (cond
      (nil? z)               nil
      (acceptable? z cursor) (if-let [z' (some-> z z/down (find-acceptable cursor ok?))]
                               z'
                               (if (ok? z)
                                 z
                                 (recur (z/right z))))
      :else                  (recur (z/right z)))))

(defn- find-object [z cursor ok?]
  (or (find-inside z cursor ok?)
      (find-acceptable z cursor ok?)))

(defn- add-embellishments [z embellishments]
  (loop [z z]
    (if (some-> z z/up z/tag embellishments)
      (recur (z/up z))
      z)))

(defmulti select :kind)

(defmethod select "element"
  [{:keys [code cursor-line cursor-column]}]
  (let [start [cursor-line cursor-column]]
    (-> (z/of-string code {:track-position? true})
        (find-object start element?)
        (add-embellishments #{:meta :meta* :reader-macro}))))

(defmethod select "form"
  [{:keys [code cursor-line cursor-column]}]
  (let [start [cursor-line cursor-column]]
    (-> (z/of-string code {:track-position? true})
        (find-object start form?)
        (add-embellishments #{:namespaced-map :meta :meta* :reader-macro
                              :syntax-quote :unquote :unquote-splicing
                              :quote}))))

(defmethod select "toplevel"
  [{:keys [code cursor-line cursor-column]}]
  (let [start [cursor-line cursor-column]]
    (-> (z/of-string code {:track-position? true})
        (z/find-depth-first (fn [z]
                              (and (acceptable? z start)
                                   (form? z))))
        (add-embellishments #{:namespaced-map :meta :meta* :reader-macro
                              :syntax-quote :unquote :unquote-splicing
                              :quote}))))

(defn- shrink [z start-offset end-offset]
  (let [[si sj] (start-position z)
        [ei ej] (end-position z)]
    [[si (+ sj start-offset)] [ei (- ej end-offset)]]))

(defn- outside-extent [z]
  [(start-position z) (end-position z)])

(defn- inside-extent [z]
  (case (z/tag z)
    (:list :map :multi-line :vector)           (shrink z 1 1)
    (:regex :set)                              (shrink z 2 1)
    (:namespaced-map :reader-macro)            (recur (-> z z/down z/right))
    (:syntax-quote :unquote :unquote-splicing) (recur (-> z z/down))
    (:token)                                   (if (string? (z/value z))
                                                 (shrink z 1 1)
                                                 (outside-extent z))
    #_otherwise                                (outside-extent z)))

(defn- selection-extent [message z]
  (when z
    (if (= "inside" (:extent message))
      (inside-extent z)
      (outside-extent z))))

(defn- response-for-select
  [message]
  (if-let [[[si sj] [ei ej]] (try (selection-extent message (select message))
                                  (catch Throwable t
                                    nil))]
    (response-for message {:status :done
                           :cursor-line si
                           :cursor-column sj
                           :anchor-line ei
                           :selection-end-column ej})
    (response-for message {:status :done})))

(defn wrap-select
  [f]
  (fn [{:keys [op transport] :as message}]
    (if (= "select" op)
      (t/send transport (response-for-select message))
      (f message))))

(set-descriptor! #'wrap-select
  {:doc "Middleware that aids an editor in finding structural objects in source."
   :handles
   {"select"
    {:doc "Find an object"
     :requires
     {"code" "The entire source of the file."
      "kind" "The kind of object (i.e. \"element\")"
      "cursor-line" "The current ones-based selection start/cursor line."
      "cursor-column" "The current ones-based selection start/cursor column."}
     :optional
     {"extent" "\"whole\" for the whole object, or \"inside\" for its insides."
      "anchor-line" "The ones-based ending line of the last character of the selection."
      "selection-end-column" "The ones-based ending column of the last character of the selection."}
     :returns
     {"cursor-line" "The ones-based starting line of the found object."
      "cursor-column" "The ones-based starting column of the found object."
      "anchor-line" "The ones-based line of the last character of the object."
      "selection-end-column" "The ones-based column of the last character of the object."}}}})
