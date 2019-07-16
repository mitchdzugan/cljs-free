(ns cljs-free.macros
  (:require [cljs-free.raw-macros :as macros]))

(defmacro varg# [& statements] `(macros/varg# ~@statements))
(defmacro whenf [& statements] `(macros/whenf ~@statements))
(defmacro deffree [& statements] `(macros/deffree ~@statements))
(defmacro deffn [& statements] `(macros/deffn ~@statements))
(defmacro deff [& statements] `(macros/deff ~@statements))
(defmacro ffn [& statements] `(macros/ffn ~@statements))
(defmacro fdo [& statements] `(macros/fdo ~@statements))

