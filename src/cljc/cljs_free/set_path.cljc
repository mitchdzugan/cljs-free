(ns cljs-free.set-path)

(defmulti grands? :type)
(defmethod grands? :grands [_] true)
(defmethod grands? :grands-reset [_] true)
(defmethod grands? :default [_] false)

(defmulti grands-reset? :type)
(defmethod grands-reset? :grands-reset [_] true)
(defmethod grands-reset? :default [_] false)

(defn elimination [round row]
  {:type :elimination
   :round round
   :row row})
(defn grands [] {:type :grands :round :GF})
(defn grands-reset [] {:type :grands-reset :round :GFR})
