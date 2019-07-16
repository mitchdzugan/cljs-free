(ns cljs-free.progression-spec)

(defn elimination [round winner?]
  {:round round
   :winner? winner?
   :key (str (if winner? "winner" "loser") "-of::" round)})
