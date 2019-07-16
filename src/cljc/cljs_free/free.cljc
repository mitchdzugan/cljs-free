(ns cljs-free.free)

(defn fail [val] (fn [] [false false nil nil (fn [_ _] val)]))
(defn pure [val] (fn [] [true false nil nil (fn [_ _] val)]))
(defn bind [m fm] (fn [] [true true m fm nil]))
(defn combine-interpreters [interpreters]
  (reduce (fn [agg {:keys [states funcs]}]
            (-> agg
                (update :states merge states)
                (update :funcs merge funcs)))
          {:states {} :funcs {}}
          interpreters))
(defn interpret [& args]
  (let [[p & interpreters] (reverse args)
        i (combine-interpreters interpreters)
        with-overrides #(apply interpret (concat interpreters %&))
        stack #?(:clj (java.util.ArrayDeque. [])
                 :cljs #js [])]
    (.push stack (fn [_] p))
    (let [fres
          (loop [res nil]
            (if (= 0 #?(:clj (.size stack)
                        :cljs (.-length stack)))
              res
              (let [[nonfail? cont? left right evalf] (((.pop stack) res))]
                (cond
                  cont? (do
                          (.push stack right)
                          (let [[nonfail? res]
                                (loop [m left]
                                  (let [[nonfail? cont? left right evalf] (m)]
                                    (cond
                                      cont? (do (.push stack right) (recur left))
                                      nonfail? [true (evalf i with-overrides)]
                                      :else [false (evalf i with-overrides)])))]
                            (if nonfail? (recur res) res)))
                  nonfail? (recur (evalf i with-overrides))
                  :else (evalf i with-overrides)))))]
      {:res fres :interpreter i})))

(defn run [& args]
  (:res (apply interpret args)))

(defn return
  ([] (pure nil))
  ([a] (pure a)))

(defn map-values [f m] (into {} (for [[k v] m] [k (f v)])))

(defn index-by [f coll]
  (persistent!
   (reduce
    (fn [ret x]
      (let [k (f x)]
        (assoc! ret k x)))
    (transient {}) coll)))

(defn of-list [[mx & mxs]]
  (if mx
    (bind mx (fn [x]
               (bind (of-list mxs)
                     (fn [xs] (pure (conj xs x))))))
    (pure [])))
(defn reader [from-state]
  (fn [{:keys [next state]}] (next (apply from-state state))))
(defn updater [from-state-and-args]
  (fn [{:keys [next state args]}]
    (next nil (apply from-state-and-args state args))))
(defn writer [field]
  (updater #(update %1 field conj %2)))
(defn setter [field-from-args value-from-args]
  (updater (fn [state & args]
             (assoc state
                    (apply field-from-args args)
                    (apply value-from-args args)))))
(defn setter-in [fields-from-args value-from-args]
  (updater (fn [state & args]
             (assoc-in state
                       (apply fields-from-args args)
                       (apply value-from-args args)))))

