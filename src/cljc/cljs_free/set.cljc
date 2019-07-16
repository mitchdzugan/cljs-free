(ns cljs-free.set
  (:require [cljs-free.entrant :as entrant]))

(defn set [path entrants & args]
  (let [winner-id (nth args 0)
        beyond-progression? (nth args 1 false)
        completed? (not (nil? winner-id))]
    {:path path
     :entrants entrants
     :winner-id winner-id
     :beyond-progression? beyond-progression?
     :completed? completed?
     :winner (if completed? (nth entrants winner-id) (entrant/winner-of path))
     :loser (if completed? (nth entrants (- 1 winner-id)) (entrant/loser-of path))}))
