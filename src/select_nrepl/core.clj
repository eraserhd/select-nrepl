(ns select-nrepl.core)

(defmulti select
  (fn [kind text start end]
    kind))

(defmethod select :element
  [_ text start end]
  [start end])
