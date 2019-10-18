(ns cljs-free.free)

(defn fail [val]
  (fn [o]
    (aset o 1 val)
    (aset o 0 :fail)))

(defn pure [val]
  (fn [o]
    (aset o 1 val)
    (aset o 0 :pure)))

(defn bind [m fm]
  (fn [o]
    (aset o 2 fm)
    (aset o 1 m)
    (aset o 0 :bind)))

(defn combine-interpreters [interpreters]
  (reduce (fn [agg {:keys [states funcs]}]
            (-> agg
                (update :states merge states)
                (update :funcs merge funcs)))
          {:states {} :funcs {}}
          interpreters))
(defn interpret [& args]
  (let [o #?(:cljs #js [0 0 0] :clj (to-array [0 0 0]))
        [p & interpreters] (reverse args)
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
              (do
                (((.pop stack) res) o)
                (case (aget o 0)
                  :bind (do
                          (.push stack (aget o 2))
                          (let [[nonfail? res]
                                (loop [m (aget o 1)]
                                  (m o)
                                  (case (aget o 0)
                                    :bind (do (.push stack (aget o 2)) (recur (aget o 1)))
                                    :pure [true (aget o 1)]
                                    :func [true ((aget o 1) i with-overrides)]
                                    :fail [false (aget o 1)]))]
                            (if nonfail? (recur res) res)))
                  :pure (recur (aget o 1))
                  :func (recur ((aget o 1) i with-overrides))
                  :fail (aget o 1)))))]
      {:res fres :interpreter i})))

(defn run [& args]
  (:res (apply interpret args)))

(defn return
  ([] (pure nil))
  ([a] (pure a)))

(defn map-values [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn index-by [f l]
  (->> l (group-by f) (map-values first))
  #_(loop [t (transient {}) [x & xs] l]
    (if xs
      (recur (assoc! t (f x) x) xs)
      (persistent! t)))
  )

(defn of-list [[mx & mxs]]
  (if mx
    (bind mx (fn [x]
               (bind (of-list mxs)
                     (fn [xs] (pure (conj xs x))))))
    (pure [])))

(defn of-list_ [[mx & mxs]]
  (if mx
    (bind mx (fn [x] (of-list_ mxs)))
    (pure nil)))

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

