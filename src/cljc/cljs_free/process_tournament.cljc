(ns cljs-free.process-tournament
  (:require [cljs-free.util :as utils]
            [cljs-free.set :as set]
            [cljs-free.set-path :as set-path]
            [cljs-free.set-result :as set-result]
            [cljs-free.entrant :as entrant]
            [cljs-free.progression :as progression]
            [cljs-free.free :as libra :refer [return]]
            #?(:clj  [cljs-free.macros :refer [varg# whenf deffree fdo deff deffn ffn]]
               :cljs [cljs-free.macros :refer-macros [varg# whenf deffree fdo deff deffn ffn]])))

(deffree process [get-env write-set write-progression set-standing wt1 wt2])

(defn add-progressions [progression-specs get-entrant]
  (->> progression-specs
       (map #(->> (get-entrant %)
                  (progression/progression %)
                  write-progression))
       libra/of-list))

(deffn create-set [path entrants beyond-progression?]
  env <- (get-env)
  let [{:keys [results-by-path progression-specs-by-round]} env
       progression-specs (get progression-specs-by-round (:round path))
       result (get results-by-path path)
       winner-id (:winner-id result)
       s (set/set path entrants winner-id beyond-progression?)]
  (write-set s)
  (add-progressions progression-specs #((if (:winner? %) :winner :loser) s))
  (whenf (and (:completed? s)
              (not (set-path/grands? path)))
    let [winner-seed (get-in s [:winner :seed-id])
         loser-seed (get-in s [:loser :seed-id])
         {:keys [round]} path
         next-round (if (< round 0) (inc round) (dec round))]
    (set-standing loser-seed (utils/place-by-round round) true)
    (set-standing winner-seed (utils/place-by-round next-round round) (= round 1)))
  (return (:winner s)))

(defn make-pool-priv [pool-config generator]
  (let [{:keys [type results entrants progression-specs]} pool-config
        results-by-path (libra/index-by :set-path results)
        progression-specs-by-round (libra/index-by :round progression-specs)
        extreme-by #(->> progression-specs
                         (apply % :round {:round 0})
                         :round)
        max-progression-round (extreme-by max-key)
        min-progression-round (extreme-by min-key)
        env {:results results
             :results-by-path results-by-path
             :entrants entrants
             :progression-specs progression-specs
             :progression-specs-by-round progression-specs-by-round
             :max-progression-round max-progression-round
             :min-progression-round min-progression-round}
        entrant-count (count entrants)
        standings (->> entrants
                       (reduce (fn [standings {:keys [seed-id]}]
                                 (if (nil? seed-id) standings
                                     (assoc! standings seed-id {:standing entrant-count
                                                                :final? false})))
                               (transient {}))
                       persistent!)
        interpreter (make-interpreter {write-set (libra/writer :sets)
                                       write-progression (libra/writer :progressions)
                                       set-standing (libra/setter-in
                                                     (varg# [:standings %1])
                                                     (varg# {:standing %2 :final? %3}))
                                       get-env (libra/reader (varg# env))
                                       wt1 (libra/writer :wt1)
                                       wt2 (libra/writer :wt2)
                                       }
                                      {:standings standings})
        res (libra/interpret interpreter generator)]
    (get-state res)))


(defn gnow []
  #?(:clj 0 :cljs (.now js/Date)))

(deff single-elim
  env <- (get-env)
  let [{:keys [entrants
               max-progression-round
               progression-specs-by-round]} env
       entrant-count (count entrants)
       seed-valid? (fn [ind]
                     (let [entrant (nth entrants ind nil)]
                       (and entrant (not (entrant/bye? entrant)))))
       f#expand (ffn [round row seed]
                  let [opponent (- (utils/p2 round) 1 seed)
                       valid? (and (seed-valid? seed) (seed-valid? opponent))
                       res {:valid? valid? :seed seed}]
                  (and (not valid?) (> round max-progression-round)) --> (return res)
                  expand0 <- (f#expand (inc round) (* 2 row) seed)
                  expand1 <- (f#expand (inc round) (inc (* 2 row)) opponent)
                  let [already-beyond-progression? (or (:beyond-progression? expand0)
                                                       (:beyond-progression? expand1))
                       progression-specs (get progression-specs-by-round round)
                       res (assoc res :beyond-progression? (or already-beyond-progression?
                                                               (some :winner? progression-specs)))
                       build-entrant (fn [{:keys [valid? seed entrant]}]
                                        (if valid? entrant (nth entrants seed nil)))
                       path (set-path/elimination round row)
                       entrants [(build-entrant expand0) (build-entrant expand1)]]
                  (not valid?) --> (fdo
                                    (add-progressions progression-specs
                                                      (varg# (entrant/bye)))
                                    (return res))
                  next-entrant <- (create-set path entrants already-beyond-progression?)
                  (return (assoc res :entrant next-entrant)))]
  (< entrant-count 2) --> (return)
  (f#expand 1 0 0))

(def wf-set-path (set-path/elimination 1 0))
(def lf-set-path (set-path/elimination -1 0))

(deff double-elim
  env <- (get-env)
  let [{:keys [standings progressions] winners-sets :sets} (make-pool-priv env single-elim)
       winners-sets-by-path (libra/index-by :path winners-sets)
       max-depth (->> winners-sets
                      (apply max-key #(-> % :path :round) {:path {:round 0}})
                      :path :round)
       f#expand (ffn [depth row drop-pivot]
                     let [drop-path (set-path/elimination (inc depth) drop-pivot)
                          drop-set (get winners-sets-by-path drop-path)
                          drop-set? (not (nil? drop-set))
                          feeders? (< depth max-depth)
                          res {:drop-set? drop-set? :feeders? feeders?}]
                     (or (not drop-set?) (not feeders?)) --> (return res)
                     let [next-drop-pivot (* 2 (- (utils/p2 depth) 1 drop-pivot))]
                     expand0 <- (f#expand (inc depth) (* 2 row) next-drop-pivot)
                     expand1 <- (f#expand (inc depth) (inc (* 2 row)) (inc next-drop-pivot))
                     let [feeders? (or (:drop-set? expand0) (:drop-set? expand1))
                          all-feeders? (and (:drop-set? expand0) (:drop-set? expand1))
                          res (assoc res :feeders? feeders?)]
                     (not feeders?) --> (return res)
                     let [get-needs-set? #(and (:drop-set? %) (not (:feeders? %)))
                          needs-set0? (get-needs-set? expand0)
                          needs-set1? (get-needs-set? expand1)
                          next-drop-path0 (set-path/elimination (+ 2 depth) next-drop-pivot)
                          next-drop-path1 (set-path/elimination (+ 2 depth) (inc next-drop-pivot))
                          get-drop-path-entrant #(let [s (get winners-sets-by-path %)]
                                                   (if (nil? s)
                                                     (entrant/loser-of %)
                                                     (:loser s)))]
                     layer1-entrant <- (whenf all-feeders?
                                         let [layer2-path (set-path/elimination (- (* -2 depth) 2) row)
                                              entrants [(if needs-set0? (get-drop-path-entrant next-drop-path0) (:entrant expand0))
                                                        (if needs-set1? (get-drop-path-entrant next-drop-path1) (:entrant expand1))]]
                                         (create-set layer2-path entrants false))
                     let [layer1-path (set-path/elimination (dec (* -2 depth)) row)
                          entrants [(get-drop-path-entrant drop-path)
                                    (or layer1-entrant (get-drop-path-entrant (if needs-set0? next-drop-path0 next-drop-path1)))]]
                     next-entrant <- (create-set layer1-path entrants false)
                     (return (assoc res :entrant next-entrant)))]
  (->> winners-sets (map write-set) libra/of-list)
  (->> standings
       seq
       (map (fn [[seed-id {:keys [standing]}]]
              (set-standing seed-id
                            (max 2 (inc (* 2 (dec standing))))
                            false)))
       libra/of-list)
  expandl <- (f#expand 0 0 0)
  [lf-entrant (:entrant expandl)
   wf-entrant (->> wf-set-path
                   (get winners-sets-by-path)
                   :winner)
   gf-entrants [wf-entrant lf-entrant]]
  gf-winner <- (create-set (set-path/grands) gf-entrants false)
  (not (entrant/filled? gf-winner)) --> (return)
  [reset? (not (= gf-winner wf-entrant))]
  gfr-winner <- (whenf reset?
                       (create-set (set-path/grands-reset) gf-entrants false))
  [done? (or (and (not reset?)
                  (entrant/filled? gf-winner))
             (entrant/filled? gfr-winner))]
  (not done?) --> (return)
  [opposite {wf-entrant lf-entrant lf-entrant wf-entrant}
   winner (if reset? gfr-winner gf-winner)
   loser (get opposite winner)]
  (set-standing (:seed-id winner) 1 true)
  (set-standing (:seed-id loser) 2 true)
  (return))

(defn make-pool [pool-config]
  (->> (:type pool-config)
       (get {:DE double-elim :SE single-elim})
       (make-pool-priv (update pool-config :entrants #(if (vector? %) % (vec %))))))

(defn t []
  (make-pool {:entrants (map #(entrant/seed %) (range 50000))
              #_ [(entrant/seed 0) (entrant/seed 1) (entrant/seed 2) (entrant/seed 3)
                         (entrant/seed 4) (entrant/seed 5) (entrant/seed 6) (entrant/seed 7)]
              :results [
                        ; (set-result/result nil (set-path/elimination 3 3) 0)
                        ; (set-result/result nil (set-path/elimination 3 2) 0)
                        ; (set-result/result nil (set-path/elimination 3 1) 0)
                        ; (set-result/result nil (set-path/elimination 3 0) 0)
                        ; (set-result/result nil (set-path/elimination 2 1) 0)
                        ; (set-result/result nil (set-path/elimination 2 0) 0)
                        ; (set-result/result nil (set-path/elimination 1 0) 0)
                        ; (set-result/result nil (set-path/elimination -4 0) 0)
                        ; (set-result/result nil (set-path/elimination -4 1) 0)
                        ; (set-result/result nil (set-path/elimination -3 0) 0)
                        ; (set-result/result nil (set-path/elimination -3 1) 0)
                        ; (set-result/result nil (set-path/elimination -2 0) 0)
                        ; (set-result/result nil (set-path/elimination -1 0) 0)
                        ; (set-result/result nil (set-path/elimination -1 0) 0)
                        ; (set-result/result nil (set-path/grands) 1)
                        ; (set-result/result nil (set-path/grands-reset) 1)
                        ]
              :type :DE})
  nil)
