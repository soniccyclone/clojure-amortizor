(ns clojure-amortizor.core
  (:require [nixnomad.brain.core :refer [spark]]
            [clojure-amortizor.loan :refer :all]
            [clojure-amortizor.person :refer :all]
            [clojure.java-time :as jt])
  (:gen-class))

(defn process-raise
  [{:keys [annual-salary annual-raise-percent percent-of-raise-for-repayment] :as person}]
  (let [raise (* annual-salary annual-raise-percent)]
    (assoc person :annual-salary (+ annual-salary raise) :extra-loan-payment (* percent-of-raise-for-repayment raise))))

(defn process-salary
  [{:keys [annual-raise-month] :as person} month]
  (if (= month annual-raise-month)
    (process-raise person)
    person))

(defn process-month
  [{:keys [extra-loan-payment loans salary]} month]
  (list extra-loan-payment loans salary month))

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

(comment
  (let [loan (create-loan "test-loan" 0.025 100 0 5)
        inactive-loan (dissoc (create-loan "bad-loan" 0.025 99 0 5) :activ)
        loans [loan inactive-loan]
        person (create-person 100 loans 100 1 0.03 1)]
    (process-month person "teetoo")
    (->> loan
         calculate-daily-interest
         (get-accrued-interest 31))
    (applicable-loans person)
    (:loans person)
    (map :principal-balance (:loans person))
    (reduce #(if (< (:principal-balance %2) (:principal-balance %1)) %2 %1)
            loans)
    (find-loan-for-extra-payment loans)
    (apply min (map :principal-balance loans))

    (total-debt person))


  (def v [1 2 3 4 0 5])

  (reduce (fn [[curr-min min-idx curr-idx] val]
            (if (< val curr-min)
              [val curr-idx (inc curr-idx)]
              [curr-min min-idx (inc curr-idx)]))
          [(first v) 0 0]
          v)
  :rcf)