(ns cljs-free.raw-macros
  (:require [cljs-free.free :refer [bind pure]]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(defmacro fdo [& statements]
  (let [statements (remove #(= 'let %) statements)
        [standard guarded] (split-with #(not (= % '-->)) statements)
        needs-guard? (> (count guarded) 1)
        guard-point (-> standard count dec)
        with-guard (reduce
                    (fn [statements curr]
                      (if (and (= (count statements) guard-point) needs-guard?)
                        (conj statements
                              `(if ~curr
                                 ~(nth guarded 1)
                                 (fdo ~@(drop 2 guarded))))
                        (conj statements curr)))
                    []
                    standard)]
    (letfn [(make-fdo [statements]
              (let [[curr & statements] statements
                    [arrow monad & other-statements] statements]
                (cond
                  (nil? statements) curr
                  (vector? curr) (let [add-rec-group (fn [forms form rec-group]
                                                       (concat forms
                                                               `(~(gensym) (do ~@(map (fn [[f a]] `(reset! ~a ~f))
                                                                                       rec-group)))
                                                               (if (nil? form) [] [form])))
                                       {:keys [sym-map forms rec-group]} (reduce
                                                                          (fn [agg form]
                                                                            (let [sym-name (if (symbol? form) (name form) "")
                                                                                  {:keys [name? sym-map rec-group forms]} agg
                                                                                  is-rec-name (and name?
                                                                                                   (string/starts-with? sym-name "f#"))
                                                                                  needs-resets (and name?
                                                                                                    (> (count rec-group) 0)
                                                                                                    (not is-rec-name))
                                                                                  f-sym (if is-rec-name (gensym sym-name) nil)
                                                                                  a-sym (if is-rec-name (gensym sym-name) nil)]
                                                                              {:name? (not name?)
                                                                               :sym-map (if is-rec-name
                                                                                          (merge sym-map {form `(deref ~a-sym)})
                                                                                          sym-map)
                                                                               :rec-group (cond
                                                                                            (not name?) rec-group
                                                                                            is-rec-name (conj rec-group [f-sym a-sym])
                                                                                            :else nil)
                                                                               :forms (cond
                                                                                        is-rec-name (concat forms
                                                                                                            `[~a-sym (atom nil)
                                                                                                              ~f-sym])
                                                                                        needs-resets (add-rec-group forms form rec-group)
                                                                                        :else (concat forms [form]))}))
                                                                          {:name? true :forms []}
                                                                          curr)
                                       forms (add-rec-group forms nil rec-group)
                                       forms (walk/postwalk #(get sym-map % %) forms)
                                       statements (walk/postwalk #(get sym-map % %) statements)]
                                   `(let [~@forms] ~(make-fdo statements)))
                  (= arrow (symbol '<-)) `(bind ~monad (fn [~curr] ~(make-fdo other-statements)))
                  :else `(bind ~curr (fn [~(gensym)] ~(make-fdo statements))))))]
      (make-fdo with-guard))))

(defn next-from-i-and-sym
  ([i sym] nil)
  ([i sym res] res)
  ([i sym res new-state]
   (reset! (get (:states i) sym) new-state)
   res))

(defn make-generic-interpreter [sym]
  (fn [& args]
    (let [funcs (nth args 0 {})
          initial-state (nth args 1 nil)]
      {:states {sym (atom initial-state)}
       :funcs (reduce-kv #(assoc %1 [sym %2] %3) {} funcs)})))

(defn get-state [ires sym]
  @(-> ires :interpreter :states (get sym)))

(defmacro deffree [name funcs]
  (let [sym (gensym name)
        int-name (symbol (str "make-interpreter"))
        state-name (symbol (str "get-state"))
        fs (map (fn [func]
                  (let [func-name (.toString func)
                        special? (fn [c] (get #{\! \-} c))
                        special-chars (take-while special? func-name)
                        private? (some #(= \- %) special-chars)
                        fun? (not (some #(= \! %) special-chars))
                        func-name (->> func-name (drop-while special?) (apply str))
                        f-sym (symbol func-name)
                        args-sym (if fun? (gensym "args") nil)
                        definition `(~(if fun? 'defn 'def)
                                     ~@(if private?
                                         [(with-meta f-sym (assoc (meta f-sym) :private true))]
                                         [f-sym])
                                     ~@(if fun? [`[& ~args-sym]] []))
                        ]
                    `(~@definition
                       (fn [o#]
                         (let [evalf#
                               (fn [i# with-overrides#]
                                 ((get (:funcs i#) ['~sym ~f-sym])
                                  {:next (partial next-from-i-and-sym i# '~sym)
                                   :interpret with-overrides#
                                   :interpret-with-overrides with-overrides#
                                   :args ~args-sym
                                   :state @(get (:states i#) '~sym)}))]
                           (aset o# 1 evalf#)
                           (aset o# 0 :func))))))
                funcs)]
    `(do
       (def ~name '~sym)
       (def ~int-name (fn [& args#] (apply (make-generic-interpreter '~sym) args#)))
       (def ~state-name (fn [i-res#] (get-state i-res# '~sym)))
       ~@fs)))

(defn add-fdo [sym]
  (fn [& statements]
    (let [fn? (= sym 'fn)
          defn? (= sym 'defn)
          n? (or defn? fn?)
          docstring? (and (not fn?) (string? (nth statements 1)))
          take-n (+ 1 (if docstring? 1 0) (if fn? -1 0))
          untouched (take take-n statements)
          add-to-fn (fn [[args & statements]]
                      `(~args (fdo ~@statements)))
          rest (drop take-n statements)
          updated (cond
                    (and n? (vector? (first rest))) (add-to-fn rest)
                    n? (map add-to-fn rest)
                    :else `((fdo ~@rest)))]
      `(~sym ~@untouched ~@updated))))

(defmacro ffn [& statements]
  (apply (add-fdo 'fn) statements))

(defmacro deff [& statements]
  (apply (add-fdo 'def) statements))

(defmacro deffn [& statements]
  (apply (add-fdo 'defn) statements))

(defmacro whenf [bool & statements]
  `(if ~bool (fdo ~@statements) (pure nil)))

(defn parse-int [s default]
  (try
    #?(:cljs (js/parseInt s 10)
       :clj (Integer/parseInt s))
    #?(:cljs (catch js/Object e default)
       :clj (catch Exception e default))))

(defmacro varg# [statement]
  (let [args (gensym "args")]
    `(fn [& ~args]
       ~(->> [statement]
             (walk/postwalk (fn [sym]
                              (let [sym-name (if (symbol? sym) (name sym) "")]
                                (if (string/starts-with? sym-name "%")
                                  `(nth ~args ~(-> sym-name (subs 1) (parse-int -1) dec) nil)
                                  sym))))
             first))))
