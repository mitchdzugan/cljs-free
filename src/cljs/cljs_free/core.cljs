(ns cljs-free.core
  (:require
   [reagent.core :as reagent :refer [atom]]
   [clerk.core :as clerk]
   [libra.dom :as dom]
   [cljs-free.app :as app]
   [cljs-free.free :refer [bind pure interpret fail]]
   [cljs-free.process-tournament :as pt]
   [cljs-free.raw-macros :refer-macros [deffree fdo ffn]]))

;; Routes

(defn home-page []
  [dom/of-env-and-free {:signals {}} app/app])

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (clerk/initialize!)
  (mount-root))
