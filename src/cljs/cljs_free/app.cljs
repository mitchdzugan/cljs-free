(ns cljs-free.app
  (:require
   [libra.frp :as frp]
   [cljs-free.macros :refer-macros [ui fnui defui defnui]]
   [cljs-free.util :as utils]
   [cljs-free.process-tournament :as pt]
   [cljs-free.entrant :as entrant]
   [cljs-free.set-path :as set-path]
   [cljs-free.set-result :as set-result]
   [libra.dom :as dom]
   [libra.core :as libra :refer [return] :refer-macros [varg# whenf deffree fdo deff deffn ffn]]))

;; probably lib

(deffn reduce-event [e & args]
  (keyword? e) --> (apply dom/reduce-event e args)
  res <- (apply dom/reduce-event ::trap-city
                (concat args [(fdo
                               (dom/join-event ::trap-city e)
                               (dom/bind-signal ::trap-city (ffn [s]
                                                                 (dom/div {} "")
                                                                 (return s))))]))
  (return (frp/bind res (fn [s] s))))

(defn bind-signal [& args]
  (apply dom/bind-signal args))

(defn onvalue [elres]
  (->> (dom/onchange elres)
       (frp/fmap #(.. % -target -value))))

(defn form [dom-creator]
  (ffn [& args]
       let [init (nth args 0)
            attr-arg (nth args 1)
            event-of-res (nth args 2 onvalue)
            body (nth args 3 nil)
            attr-of-value (if (fn? attr-arg)
                            attr-arg (varg# attr-arg))]
       ss <- (dom/reduce-event ::input (varg# %2) init
                               (dom/bind-signal ::input
                                                (ffn [v]
                                                     i <- (dom-creator (merge (attr-of-value v) {:value v}) body)
                                                     (dom/join-event ::input (event-of-res i))
                                                     (return v))))
       (return (frp/bind ss (fn [s] s)))))

(def input (form dom/input))
(defn select-raw [& args] (apply dom/write-tag :select args))
(defn option [& args] (apply dom/write-tag :option args))
(defn h2 [& args] (apply dom/write-tag :h2 args))
(def select (form select-raw))

;; end probably lib

(defn number [default attrs]
  (input default attrs
         (fn [elres]
           (->> (onvalue elres)
                (frp/fmap #(or (utils/parse-int %) nil))))))

(deffn div_ [& args]
  res <- (apply dom/div args)
  (return (:inner-res res)))

(defn make-label [num-entrants round row]
  (let [winners? (>= round 0)
        round-base (if winners?
                     (+ 1
                        (min
                         (- num-entrants 2)
                         (* (dec (utils/p2 round))
                            2))
                        (dec (utils/p2 (dec round))))
                     (let [p (-> round inc (quot -2))]
                       (- (* (if (even? round) 4 3)
                             (utils/p2 p))
                          2)))
        sets-after (+ 1 row round-base)
        get-letter (fn [n] (nth "abcdefghijklmnopqrstuvwxyz" n))]
    (letfn [(get-letters
              [n] (if (= 0 n) "" (str (get-letters (quot (dec n) 26))
                                      (get-letter (mod (dec n) 26)))))]
      (get-letters sets-after))))

(deffn bracket [{:keys [num-entrants bracket-type results]}]
  let [{:keys [sets standings]} (pt/make-pool {:entrants (->> (range num-entrants)
                                                              (map entrant/seed))
                                               :results results
                                               :type bracket-type})
       _ (println "Rendering...")
       extreme-by #(->> sets
                        (filter (fn [set] (-> set :path :round number?)))
                        (apply % (fn [set] (-> set :path :round)) {:path {:round 0}})
                        :path :round)
       max-round (extreme-by max-key)
       min-round (extreme-by min-key)
       sets-by-round (group-by #(get-in % [:path :round]) sets)
       winners-set-height (#(-> % (* 4) (/ 3) js/Math.log2 (- 2) js/Math.ceil utils/p2) num-entrants)
       losers-set-height (#(-> % (/ 5) js/Math.log2 js/Math.ceil utils/p2) num-entrants)
       winners-rounds (concat (range max-round 0 -1) [:GF :GFR])
       losers-rounds (concat (range min-round 0))
       winners-rounds (filter #(not (empty? (get sets-by-round %))) winners-rounds)
       losers-rounds (filter #(not (empty? (get sets-by-round %))) losers-rounds)
       render-round (ffn [round]
                         let [raw-round round
                              sets (get sets-by-round round)
                              round (if (number? round) round 0)
                              winners? (>= round 0)
                              max-set-height (utils/p2 (if winners?
                                                         (max 0 (dec round))
                                                         (quot (* -1 (inc round)) 2)))
                              sets-by-chunk (group-by #(quot (get-in % [:path :row] 0) 2) sets)
                              half? (and (> max-set-height 1)
                                         (not (or winners? (even? round)))
                                         (every? (fn [[chunk sets]] (< (count sets) 2)) sets-by-chunk))
                              set-height (min (if winners? winners-set-height losers-set-height)
                                              (quot max-set-height (if half? 2 1)))
                              round-full-height (if winners? winners-set-height losers-set-height)
                              round-full-height-px (* 80 round-full-height)
                              center-of-row #(-> %
                                                 (* set-height)
                                                 (quot max-set-height)
                                                 (* 2)
                                                 ((fn [a]
                                                    (if (and (= set-height max-set-height)
                                                             (= set-height round-full-height)
                                                             (= 1 (count (get sets-by-chunk (quot % 2))))
                                                             (or winners? (odd? round))
                                                             (> (* round round) 1))
                                                      a
                                                      (inc a)
                                                      )))
                                                 (* round-full-height-px)
                                                 (quot (* 2 set-height)))
                              render-set (ffn [set]
                                              let [path (:path set)
                                                   row (or (:row path) 0)
                                                   center (center-of-row row)
                                                   render-entrant (ffn [id entrant]
                                                                       d <- (dom/div {:className (str "entrant"
                                                                                                      (if (= :dep (:type entrant))
                                                                                                        " unfilled" "")
                                                                                                      (if (= entrant (:winner set))
                                                                                                        " winner" "")
                                                                                                      (if (= entrant (:loser set))
                                                                                                        " loser" "")
                                                                                                      )}
                                                                                     (if (= :seed (:type entrant))
                                                                                       (str "player " (inc (:seed-id entrant)))
                                                                                       (str (if (:winner? entrant)
                                                                                              "winner" "loser")
                                                                                            " of "
                                                                                            (make-label
                                                                                             num-entrants
                                                                                             (:round (:dep entrant))
                                                                                             (:row (:dep entrant))))))
                                                                       (dom/join-event
                                                                        ::results
                                                                        (frp/fmap (varg#
                                                                                   {:type :conj
                                                                                    :v
                                                                                    (set-result/result nil
                                                                                                       (cond
                                                                                                         (= raw-round :GF) (set-path/grands)
                                                                                                         (= raw-round :GFR) (set-path/grands-reset)
                                                                                                         :else (set-path/elimination raw-round row))
                                                                                                       id)})
                                                                                  (dom/onclick d))))]
                                              (div_ {:className "set"
                                                     :style {:top (str center "px")}}
                                                    (fdo
                                                     (dom/div {:className "setName"}
                                                              (make-label num-entrants round row))
                                                     (dom/div {:className "entrants"}
                                                              (libra/of-list (map-indexed render-entrant (:entrants set))))
                                                     )
                                                    ))]
                         (div_ {:className "round" :style {:height (str round-full-height-px "px")}}
                               (libra/of-list (map render-set sets))))
       render-rounds (ffn [rounds]
                          (div_ {:className "bracket"}
                                (libra/of-list (map render-round rounds))))]
  (div_ {:className "brackets"}
        (fdo
         (whenf (not-empty losers-rounds)
                (h2 "Winners Bracket"))
         (render-rounds winners-rounds)
         (whenf (not-empty losers-rounds)
                (h2 "Losers Bracket")
                (render-rounds losers-rounds)))))

(defui controls
  :<div {:className "controls"} :=
    :<span "Entrants: " :>
    ;; bind the value of a number input with initial
    ;; value 8 to the signal num-entrantsS
    :<number 8 {:type "text"} :num-entrantsS:>
    :<span "Bracket Type: " :>
    ;; bind the value of a select input with initial
    ;; value :DE to the signal bracket-typeS
    :<select :DE {} #(->> (onvalue %)
                          (frp/fmap keyword)) :=
      :<option {:value :DE} "Double Elim" :>
      :<option {:value :SE} "Single Elim" :>
    :bracket-typeS:>
  :>
  ;; zip the signals num-entrantsS and bracket-typeS
  ;; into a single signal containg an object with
  ;; their values as the elements and return
  let [res (frp/zip-with #(-> {:num-entrants %1
                               :bracket-type %2})
                         num-entrantsS
                         bracket-typeS)]
  (return res))

(defui app
  ;; bind the result of controls UI to the signal configS
  :<controls :configS:!>
  ;; reduce-event takes 3 args and children. The first arg is 
  ;; an identifier used for later reference. The second is a
  ;; reducing function that takes the current state, a fired event
  ;; and returns the updated state based on that event. The
  ;; third arg is the initial state.
  ;; 
  ;; The children are then evaluated and any calls to dom/join-event
  ;; with this key will cause their fired events to trigger the
  ;; reducer function and update the state
  ;; 
  ;; This state is kept in a signal which can be accessed from any
  ;; of the children using dom/get-signal key
  ;; 
  ;; This reduce-event tracks the set-result list. It handles 2 types
  ;; of events, a :conj type in which it adds the payload toe he list
  ;; of set-results and a :reset type that sets the set-results back to
  ;; an empty list.
  :<reduce-event ::results #(cond
                              (= :conj (:type %2)) (conj %1 (:v %2))
                              (= :reset (:type %2)) []
                              :else %1) [] :=
    ;; fires the :reset type event whenever the config (numEntrants/bracketType) changes
    :<dom/join-event ::results (frp/fmap (varg# {:type :reset}) (frp/changed configS)) :>
    ;; extract the ::results signal and bind it to resultsS
    :<dom/get-signal ::results :resultsS:>
    ;; merge the resultsS and configS signals into a single signal value
    ;; containing them as elements. Then bind the UI function `bracket` to
    ;; that signal (IE run function `bracket`) with the value of the signal
    ;; every time it changes.
    ;;
    ;; This code compiles to stuff looking like the functions above these commented
    ;; ones. If you look you'll see another dom/join-event for adding set results
    ;; when clicking on sets.
    :<bind-signal (frp/zip-with #(merge %1 {:results %2}) configS resultsS) bracket :>
  :>)

