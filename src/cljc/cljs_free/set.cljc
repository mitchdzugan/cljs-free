(ns cljs-free.set
  (:require [cljs-free.entrant :as entrant]))

(defn set [path entrants & args]
  (let [winner-id (nth args 0)
        beyond-progression? (nth args 1 false)
        completed? (not (nil? winner-id))
        [winner loser] (cond
                         (not completed?) [(entrant/winner-of path) (entrant/loser-of path)]
                         (= 0 winner-id) entrants
                         :else (reverse entrants))]
    {:path path
     :entrants entrants
     :winner-id winner-id
     :beyond-progression? beyond-progression?
     :completed? completed?
     :winner winner
     :loser loser}))
