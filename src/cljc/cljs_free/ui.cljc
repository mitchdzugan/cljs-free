(ns cljs-free.ui
  (:require
   [cljs-free.free :as libra :refer [return]]
   #?(:clj  [cljs-free.macros :refer [deffree deffn ffn]]
      :cljs [cljs-free.macros :refer-macros [deffree deffn ffn]])))

(deffree ui [!get-env
             keyed
             bind-signal
             join-event
             reduce-event
             collect-events
             write-tag
             -set-env])

(deffn with-env [env inner]
  curr <- get-env
  (set-env env)
  res <- inner
  (set-env curr)
  (return res))

(deffn with-signal [k s* inner]
  env <- get-env
  (with-env (update env :signals conj s*) inner))

(deffn get-signal [k]
  env <- get-env
  (return (get-in env [:signals k])))

(deffn with-sys [k reducer init inner]
  (collect-events k
                  (ffn [e*]
                       s* <- (reduce-event e* reducer init)
                       (with-signal k s* inner))))

(defn make-react-interpreter [env-init]
  (make-interpreter
   {get-env (fn [{:keys [next state]}]
              (next (:env state)))
    keyed (fn [{:keys [next state args]}]
            (let [[next-key] args]
              (next nil (assoc-in state [:env :keyed] next-key))))
    ; S[a] -> a -> Dom[b] -> S[b]
    bind-signal (fn [{:keys [next state args interpret-with-overrides]}] ; TODO
                  )
    join-event (fn [{:keys [next state args interpret-with-overrides]}] ; TODO
                 )
    reduce-event (fn [{:keys [next state args interpret-with-overrides]}] ; TODO
                   )
    collect-events (fn [{:keys [next state args interpret-with-overrides]}] ; TODO
                     )
    write-tag (fn [{:keys [next state args interpret-with-overrides]}] ; TODO
                     )
    set-env (fn [{:keys [args next state]}]
                 (let [[new-env] args]
                   (next nil (assoc state :env new-env))))}
   {:env env-init}))
