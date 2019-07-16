(ns cljs-free.set-result)

(defn result [pool-path set-path winner-id]
  {:winner-id winner-id
   :set-path set-path
   :pool-path pool-path})
