(ns clojure-amortizor.core
  (:require [nixnomad.brain.core :refer [spark]])
  (:gen-class))

(defn run-handler [{:keys [run-handler data-bag]}]
  (let [{:keys [fps counter counter-print-step]} data-bag]
    (cond
      (> counter 1000)
      [false {:run-handler run-handler :data-bag (merge data-bag {:counter (inc counter)})}]
      (zero? (mod counter counter-print-step))
      (do
        (println (str "FPS: " fps))
        [true {:run-handler run-handler :data-bag (merge data-bag {:counter (inc counter)})}])
      :else
      [true {:run-handler run-handler :data-bag (merge data-bag {:counter (inc counter)})}])))

(defn -main
  "Example application copied from brain using brain as a library"
  [& _args]

  (spark {:run-handler run-handler :data-bag {:counter 0 :counter-print-step 1}}))