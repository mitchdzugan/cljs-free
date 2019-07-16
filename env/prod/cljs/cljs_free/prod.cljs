(ns cljs-free.prod
  (:require [cljs-free.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
