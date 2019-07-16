(ns cljs-free.pool-path)

(defn pool-path [segment pool-id]
  {:segment segment :pool-id pool-id
   :key (str "pool-path::" segment ":" pool-id)})
