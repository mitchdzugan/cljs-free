(ns cljs-free.macros
  (:require
   [clojure.string :as str]
   [libra.dom :as dom]
   [clojure.walk :as walk]
   [cljs-free.raw-macros :as macros]
   #?(:clj  [libra.core :as libra :refer [return]]
      :cljs [libra.core :as libra :refer [return]])
   ))

(defmacro varg# [& statements] `(macros/varg# ~@statements))
(defmacro whenf [& statements] `(macros/whenf ~@statements))
(defmacro deffree [& statements] `(macros/deffree ~@statements))
(defmacro deffn [& statements] `(macros/deffn ~@statements))
(defmacro deff [& statements] `(macros/deff ~@statements))
(defmacro ffn [& statements] `(macros/ffn ~@statements))
(defmacro fdo [& statements] `(macros/fdo ~@statements))


(def smap {
           "div" (symbol "libra.dom" "div")
           "span" (symbol "libra.dom" "span")
           })
(defn make-ui [& forms]
  (let [{:keys [pre bound-syms]}
        (reduce
         (fn [agg sym]
           (let [{:keys [inner-args
                         bound-syms
                         otc
                         mfm
                         body
                         pre
                         args
                         in-args?
                         in-body?
                         just-closed?]} agg
                 is-keyword? (keyword? sym)
                 sym-name (if is-keyword?
                            (str
                             (if (namespace sym)
                               (str (namespace sym) "/") "")
                             (name sym))
                            "")
                 is-open-tag? (str/starts-with? sym-name "<")
                 tag (if is-keyword? (subs sym-name 1) "")
                 is-end-args? (str/ends-with? sym-name "=")
                 is-end? (or (str/ends-with? sym-name ">")
                             (str/ends-with? sym-name "!>"))
                 is-f? (not (str/ends-with? sym-name "!>"))
                 pre (if (and just-closed? (not is-end?))
                       (let [env-sym (gensym "env")]
                         (conj (conj (conj (conj pre env-sym) '<-) `(dom/get-env))
                               `[~@(reduce-kv
                                    (fn [agg sname sym]
                                      (concat agg [sym `(get ~env-sym ~(keyword sname))]))
                                    []
                                    bound-syms)]))
                       pre)]
             (if (and is-end? (= otc 1))
               (let [results (-> sym-name (str/split #":") drop-last)
                     [result] results
                     {new-bound-syms :bound-syms ui-body :res} (apply make-ui (reverse body))
                     bound-syms (merge bound-syms new-bound-syms)]
                 {:otc 0
                  :just-closed? true
                  :bound-syms (if result
                                (merge bound-syms {(symbol result) (gensym result)})
                                bound-syms)
                  :pre (let [fm
                             (if is-f?
                               `(~mfm ~@(reverse args)
                                 ~@(if (> (count body) 0)
                                     [(if (> (count inner-args) 0)
                                        `(fn [~@(map symbol inner-args)]
                                           ~ui-body)
                                        ui-body)]
                                     []))
                               mfm)

                             wrapped-fm
                             (if result
                               `(libra/bind (dom/get-env)
                                            (fn [env#]
                                              (libra/bind ~fm
                                                          (fn [res#]
                                                            (libra/bind (dom/set-env (merge env# {~(keyword result) res#}))
                                                                        (fn [ignore#]
                                                                          (libra/return res#)))))))
                               fm)]
                         (conj pre wrapped-fm))})
               {:bound-syms bound-syms
                #_(if is-end?
                  (let [results (-> sym-name (str/split #":") drop-last)
                        [result] results]
                    (if result
                      (merge {(symbol result) (gensym result)} bound-syms)
                      bound-syms))
                  bound-syms)
                :just-closed? false
                :inner-args (if (and (= otc 1) is-end-args?)
                              (-> sym-name (str/split #":") drop-last)
                              inner-args)
                :otc (cond
                       is-open-tag? (inc otc)
                       is-end? (dec otc)
                       :else otc)
                :mfm (if (and is-open-tag? (= otc 0))
                       (get smap tag
                            (if (str/includes? tag "/")
                              (symbol (-> tag (str/split #"/") first)
                                      (-> tag (str/split #"/") reverse first))
                              (symbol tag)))
                       mfm)
                :args (if (and in-args?
                               (= otc 1)
                               (not is-end-args?)
                               (not is-end?))
                        (conj args sym) args)
                :pre (if (and (not in-args?)
                              (not in-body?)
                              (not is-open-tag?))
                       (conj pre sym) pre)
                :body (if (and in-body?
                               (not (and is-end?
                                         (= otc 1))))
                        (conj body sym) body)
                :in-args? (or (and in-args?
                                   (not is-end-args?)
                                   (not is-end?))
                              is-open-tag?)
                :in-body? (or (and in-body?
                                   (not (and is-end?
                                             (= 1 otc))))
                              (and in-args? is-end-args?))})))
         {:otc 0}
         forms)
        pre (walk/postwalk #(get bound-syms % %) pre)]
    {:bound-syms bound-syms
     :res `(libra/fdo ~@(reverse pre))}))

(defmacro ui [& forms]
  (:res (apply make-ui forms)))

(defn add-fdo [sym]
  (fn [& statements]
    (let [fn? (= sym 'fn)
          defn? (= sym 'defn)
          n? (or defn? fn?)
          docstring? (and (not fn?) (string? (nth statements 1)))
          take-n (+ 1 (if docstring? 1 0) (if fn? -1 0))
          untouched (take take-n statements)
          add-to-fn (fn [[args & statements]]
                      `(~args (ui ~@statements)))
          rest (drop take-n statements)
          updated (cond
                    (and n? (vector? (first rest))) (add-to-fn rest)
                    n? (map add-to-fn rest)
                    :else `((ui ~@rest)))]
      `(~sym ~@untouched ~@updated))))

(defmacro fnui [& statements]
  (apply (add-fdo 'fn) statements))

(defmacro defui [& statements]
  (apply (add-fdo 'def) statements))

(defmacro defnui [& statements]
  (apply (add-fdo 'defn) statements))
