(ns cljs-free.util)

(defn pow [b e]
  #?(:clj (Math/pow b e)
     :cljs (.pow js/Math b e)))

(defn floor [n]
  #?(:clj (Math/round (Math/floor n))
     :cljs (.round js/Math (.floor js/Math n))))

(defn p2 [p] (->> p (pow 2) floor))

;; horribly written function but it works.tm
(defn place-by-round [round & args]
  (let [prev-round (nth args 0 0)]
    (if (>= round 0)
      (+ (if (and (< prev-round 0) (= round 0)) 1 0)
         (inc (p2 (dec round))))
      (let [round (dec round)
            pos-round (* -1 (quot round 2))
            offset (if (odd? round) (p2 (dec pos-round)) 0)]
        (+ 1 offset (p2 pos-round))))))

(def log
  #?(:clj (partial println)
     :cljs (fn [& args] (apply (aget js/console "log") args))))

(defn parse-int [s]
  #?(:cljs (let [res (js/parseInt s 10)]
             (if (js/Number.isNaN res) nil res))
     :clj nil))
