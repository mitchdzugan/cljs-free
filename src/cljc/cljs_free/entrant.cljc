(ns cljs-free.entrant)

(def dep :dep)

(defmulti filled? :type)
(defmethod filled? :seed [_] true)
(defmethod filled? :bye [_] true)
(defmethod filled? :default [_] false)

(defmulti bye? :type)
(defmethod bye? :bye [_] true)
(defmethod bye? :default [_] false)

(defn bye [] {:type :bye})
(defn progression [text] {:type :progression :text text})
(defn winner-of [path] {:type dep dep path :winner? true})
(defn loser-of [path] {:type dep dep path :winner? false})
(defn seed [seed-id] {:type :seed :seed-id seed-id})
