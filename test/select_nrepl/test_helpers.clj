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
  (let [result
        (:text (reduce
                (fn [state ch]
                  (cond-> state
                    (= start (:position state)) (update :text str \<)
                    (= end (:position state))   (update :text str \>)
                    true                        (update :text str ch)
                    (= ch \newline)             (update :position (fn [[i j]] [(inc i) 0]))
                    (not= ch \newline)          (update-in [:position 1] inc)))
                {:text ""
                 :position [1 1]}
                (concat (seq text) [\space])))]
    (subs result 0 (dec (count result)))))

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

(defn select [kind input]
  (let [server (binding [*file* nil]
                 (nrepl.server/start-server :handler handler))]
    (try
      (let [conn (nrepl/connect :port (:port server))
            client (nrepl/client conn 60000)
            session (nrepl/client-session client)
            {:keys [text start end]} (parse-input input)
            msg-seq (session {:op "select"
                              :kind kind
                              :code text
                              :selection-start-line (first start)
                              :selection-start-column (second start)
                              :selection-end-line (first end)
                              :selection-end-column (second end)})
            result (transduce (until-status "done") merge {} msg-seq)]
        (compose-output text
                        [(:selection-start-line result) (:selection-start-column result)]
                        [(:selection-end-line result) (:selection-end-column result)]))
      (finally
        (nrepl.server/stop-server server)))))
