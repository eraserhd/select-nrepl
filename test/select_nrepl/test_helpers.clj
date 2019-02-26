(ns select-nrepl.test-helpers
  (:require
   [midje.sweet :refer :all]
   [select-nrepl.core]
   [nrepl.core :as nrepl]
   [nrepl.server]))

(defn- parse-input
  [text]
  (reduce
    (fn [state ch]
      (case ch
        \|       (assoc state :cursor (:position state))
        \_       (-> state
                     (assoc :anchor (:position state))
                     (update-in [:anchor 1] dec))
        \newline (-> state
                     (update :position (fn [[i j]] [(inc i) 0]))
                     (update :text str ch))
        (-> state
          (update-in [:position 1] inc)
          (update :text str ch))))
    {:text ""
     :position [1 1]
     :cursor nil
     :anchor nil}
    (seq text)))

(defn- compose-output*bad
  [text cursor anchor]
  (:text (reduce
          (fn [state ch]
            (as-> state $
              (cond-> $ (= cursor (:position $)) (update :text str \|))
              (update $ :text str ch)
              (cond-> $ (= anchor (:position $)) (update :text str \_))
              (update-in $ [:position 1] inc)
              (cond-> $ (= ch \newline)          (update :position (fn [[i j]] [(inc i) 1])))))
          {:text ""
           :position [1 1]}
          (seq text))))

(defn- compose-output
  [text cursor anchor]
  (:text (reduce
          (fn [state ch]
            (as-> state $
              (update $ :text str ch)
              (update-in $ [:position 1] inc)
              (cond-> $ (= cursor (:position $))                       (update :text str \|))
              (cond-> $ (= anchor (:position $))                       (update :text str \_))
              (cond-> $ (= ch \newline)                                (update :position (fn [[i j]] [(inc i) 0])))
              (cond-> $ (and (= ch \newline) (= cursor (:position $))) (update :text str \|))
              (cond-> $ (and (= ch \newline) (= anchor (:position $))) (update :text str \_))))
          {:text (str (if (= [1 0] cursor) "|" "")
                      (if (= [1 0] anchor) "_" ""))
           :position [1 0]}
          (seq text))))

(facts "about compose-output"
  (fact "columns are zero-based caret offset"
    (compose-output "hello" [1 0] [1 1])   => "|h_ello"
    (compose-output "hello" [1 1] [1 0])   => "_h|ello")
  (fact "handles positions at bol"
    (compose-output "he\nllo" [2 0] [2 1]) => "he\n|l_lo")
  (fact "handles positions at eof"
    (compose-output "he" [1 2] [1 2]) => "he|_")
  (fact "zero-width selections are representable"
    (compose-output "hello" [1 2] [1 2]) => "he|_llo"))

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

(defn select [extent kind input & [extra]]
  (let [server (binding [*file* nil]
                 (nrepl.server/start-server :handler handler))]
    (try
      (let [conn (nrepl/connect :port (:port server))
            client (nrepl/client conn 60000)
            session (nrepl/client-session client)
            {:keys [text cursor anchor]} (parse-input input)
            msg-seq (session (merge {:op "select"
                                     :extent extent
                                     :kind kind
                                     :code text
                                     :cursor-line (first cursor)
                                     :cursor-column (second cursor)
                                     :anchor-line (first anchor)
                                     :anchor-column (second anchor)}
                                    extra))
            result (transduce (until-status "done") merge {} msg-seq)]
        (compose-output*bad text
                        [(:cursor-line result) (:cursor-column result)]
                        [(:anchor-line result) (:anchor-column result)]))
      (finally
        (nrepl.server/stop-server server)))))
