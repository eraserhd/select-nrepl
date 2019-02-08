(ns select-nrepl.core
  (:require
   [select-nrepl.rewrite-clj.node :as n]
   [select-nrepl.rewrite-clj.zip :as z]
   [nrepl.middleware :refer [set-descriptor!]]
   [nrepl.misc :refer [response-for]]
   [nrepl.transport :as t]))

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
            (inc columns)))]))

(defn- position<=? [a b]
  (<= (compare a b) 0))

(defn- element?
  [z]
  (case (z/tag z)
    :reader-macro               (= :token (-> z z/down z/right z/tag))
    (:token :regex :multi-line) true
    false))

(defn- form?
  [z]
  (case (z/tag z)
    (:list :vector :map :set) true
    false))

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
  [{:keys [code selection-start-line selection-start-column]}]
  (let [start [selection-start-line selection-start-column]]
    (-> (z/of-string code {:track-position? true})
        (find-object start element?)
        (add-embellishments #{:meta :meta* :reader-macro}))))

(defmethod select "form"
  [{:keys [code selection-start-line selection-start-column]}]
  (let [start [selection-start-line selection-start-column]]
    (-> (z/of-string code {:track-position? true})
        (find-object start form?)
        (add-embellishments #{:namespaced-map :meta :meta* :reader-macro
                              :syntax-quote :unquote :unquote-splicing
                              :quote}))))

(defmethod select "toplevel"
  [{:keys [code selection-start-line selection-start-column]}]
  (let [start [selection-start-line selection-start-column]]
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

(defmulti extent
  "Find the extend of the node at z, accounting for the node type
  and whether we want an \"inside\" or \"whole\" extent."
  (fn [message z]
    [(or (:extent message) "whole") (when z (z/tag z))]))

(defmethod extent :default
  [_ z]
  [(start-position z) (end-position z)])

(defmethod extent ["whole" nil]
  [_ _]
  nil)

(defmethod extent ["inside" :list]
  [message z]
  (shrink z 1 1))

(defmethod extent ["inside" :namespaced-map]
  [message z]
  (extent message (-> z z/down z/right)))

(defmethod extent ["inside" :map]
  [message z]
  (shrink z 1 1))

(defmethod extent ["inside" :multi-line]
  [message z]
  (shrink z 1 1))

(defmethod extent ["inside" :reader-macro]
  [message z]
  (extent message (-> z z/down z/right)))

(defmethod extent ["inside" :regex]
  [_ z]
  (shrink z 2 1))

(defmethod extent ["inside" :set]
  [message z]
  (shrink z 2 1))

(defmethod extent ["inside" :syntax-quote]
  [message z]
  (extent message (-> z z/down)))

(defmethod extent ["inside" :token]
  [_ z]
  (cond
   (string? (z/value z)) (shrink z 1 1)
   :else                 [(start-position z) (end-position z)]))

(defmethod extent ["inside" :unquote]
  [message z]
  (extent message (-> z z/down)))

(defmethod extent ["inside" :unquote-splicing]
  [message z]
  (extent message (-> z z/down)))

(defmethod extent ["inside" :vector]
  [_ z]
  (shrink z 1 1))

(defn- response-for-select
  [message]
  (if-let [[[si sj] [ei ej]] (try (extent message (select message))
                                  (catch Throwable t
                                    nil))]
    (response-for message {:status :done
                           :selection-start-line si
                           :selection-start-column sj
                           :selection-end-line ei
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
      "selection-start-line" "The current ones-based selection start/cursor line."
      "selection-start-column" "The current ones-based selection start/cursor column."}
     :optional
     {"extent" "\"whole\" for the whole object, or \"inside\" for its insides."
      "selection-end-line" "The ones-based ending line of the last character of the selection."
      "selection-end-column" "The ones-based ending column of the last character of the selection."}
     :returns
     {"selection-start-line" "The ones-based starting line of the found object."
      "selection-start-column" "The ones-based starting column of the found object."
      "selection-end-line" "The ones-based line of the last character of the object."
      "selection-end-column" "The ones-based column of the last character of the object."}}}})
