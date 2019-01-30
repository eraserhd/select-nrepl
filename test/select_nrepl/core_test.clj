(ns select-nrepl.core-test
  (:require
   [clojure.test :refer :all]
   [select-nrepl.core]))

(defn- parse-input
  [text]
  (reduce
    (fn [state ch]
      (case ch
        \<       (assoc state :start (:position state))
        \>       (assoc state :end (:position state))
        \newline (update state :position (fn [[i j]] [(inc i) 0]))
        (-> state
          (update-in [:position 1] inc)
          (update :text str ch))))
    {:text ""
     :position [0 0]
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
                 :position [0 0]}
                (concat (seq text) [\space])))]
    (subs result 0 (dec (count result)))))

(defn- select [kind input]
  (let [{:keys [text start end]} (parse-input input)
        [start end] (select-nrepl.core/select kind text start end)]
    (compose-output text start end)))

(deftest a-select-element
  (are [before after] (= (select :element before) after)
    ;;    Input             Output
    ;;--------------   -----------------
    "<hello>"          "<hello>"
    "  hello/<>world"  "  <hello/world>"
    " :foo/ba<>r"      " <:foo/bar>"
    " \\new<>line"     " <\\newline>"
    " \"a s<t>ring\" " " <\"a string\"> "
    " 4200<>0  x"      " <42000>  x"
    "++ #\"<>x\" ;;-"  "++ <#\"x\"> ;;-"
    " #fo<>o \"h\" "   " <#foo \"h\"> "

    "( he<>llo there)" "( <hello> there)"))
