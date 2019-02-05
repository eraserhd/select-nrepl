(ns select-nrepl.core
  (:require
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
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
            columns))]))

(defn- position<=? [a b]
  (<= (compare a b) 0))

(defn- element?
  [z]
  (case (z/tag z)
    :reader-macro               (= :token (-> z z/down z/right z/tag))
    (:token :regex :multi-line) true
    false))

(defn- acceptable?
  "A node is acceptable if it contains the cursor or starts after
  cursor.  In other words, if it ends after the cursor (inclusive)."
  [z cursor]
  (position<=? cursor (end-position z)))

(defmulti select :kind)

(defmethod select "element"
  [{:keys [code selection-start-line selection-start-column]}]
  (let [start [selection-start-line selection-start-column]]
    (-> (z/of-string code {:track-position? true})
        (z/find-depth-first (fn [z]
                              (and (acceptable? z start)
                                   (element? z)))))))

(defmulti extent
  "Find the extend of the node at z, accounting for the node type
  and whether we want an \"inside\" or \"whole\" extent."
  (fn [message z]
    [(if (:inside message) :inside :whole)
     (when z (z/tag z))]))

(defmethod extent :default
  [_ z]
  [(start-position z) (end-position z)])

(defmethod extent [:whole nil]
  [_ _]
  nil)

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
     {"selection-end-line" "The ones-based ending line of the last character of the selection."
      "selection-end-column" "The ones-based ending column of the last character of the selection."}
     :returns
     {"selection-start-line" "The ones-based starting line of the found object."
      "selection-start-column" "The ones-based starting column of the found object."
      "selection-end-line" "The ones-based line of the last character of the object."
      "selection-end-column" "The ones-based column of the last character of the object."}}}})
