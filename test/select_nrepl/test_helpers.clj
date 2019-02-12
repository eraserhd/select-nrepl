(ns select-nrepl.test-helpers
  (:require
   [select-nrepl.core]
   [nrepl.core :as nrepl]
   [nrepl.server]))

(defn- parse-input
  [text]
  (reduce
    (fn [state ch]
      (case ch
        \<       (assoc state :start (:position state))
        \>       (assoc state :end (:position state))
        \newline (-> state
                     (update :position (fn [[i j]] [(inc i) 0]))
                     (update :text str ch))
        (-> state
          (update-in [:position 1] inc)
          (update :text str ch))))
    {:text ""
     :position [1 1]
     :start nil
     :end nil}
    (seq text)))

(defn- compose-output
  [text start end]
  (:text (reduce
          (fn [state ch]
            (as-> state $
              (cond-> $ (= start (:position $)) (update :text str \<))
              (update $ :text str ch)
              (cond-> $ (= end (:position $))   (update :text str \>))
              (update-in $ [:position 1] inc)
              (cond-> $ (= ch \newline)         (update :position (fn [[i j]] [(inc i) 1])))))
          {:text ""
           :position [1 1]}
          (seq text))))

(def ^:private handler
  (nrepl.server/default-handler select-nrepl.core/wrap-select))

(defn- take-until
  "Transducer like take-while, except keeps the last value tested."
  [pred]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (let [result (rf result input)]
         (cond
          (reduced? result) result
          (pred input)      (reduced result)
          :else             result))))))

(defn- until-status
  "Process messages until we see one with a particular status."
  [status]
  (take-until #(contains? (into #{} (:status %)) status)))

(defn select [extent kind input]
  (let [server (binding [*file* nil]
                 (nrepl.server/start-server :handler handler))]
    (try
      (let [conn (nrepl/connect :port (:port server))
            client (nrepl/client conn 60000)
            session (nrepl/client-session client)
            {:keys [text start end]} (parse-input input)
            msg-seq (session {:op "select"
                              :extent extent
                              :kind kind
                              :code text
                              :cursor-line (first start)
                              :cursor-column (second start)
                              :anchor-line (first end)
                              :anchor-column (second end)})
            result (transduce (until-status "done") merge {} msg-seq)]
        (compose-output text
                        [(:cursor-line result) (:cursor-column result)]
                        [(:anchor-line result) (:anchor-column result)]))
      (finally
        (nrepl.server/stop-server server)))))
