(ns clojure-amortizor.core
  (:require [nixnomad.brain.core :refer [spark]]
            [clojure-amortizor.loan :refer :all]
            [clojure-amortizor.person :refer :all]
            [java-time.api :as jt])
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

(defn accrue-interest
  [loans]
  (map (fn [{:keys [accrued-interest principal-balance interest-rate] :as loan}]
         (assoc loan :accrued-interest
                (->> interest-rate
                     (* principal-balance)
                     (+ accrued-interest))))
       loans))

(defn bounce-extra-loan-payments
  "Mutually recursive function that marks loans with an extra-payment that can be
   made on them given the extra payment that can be made"
  [extra-payment bounced-loans bouncing-loans]
  (if (empty? bouncing-loans) bounced-loans
      (let [loan (first bouncing-loans)
            leftover-principal-balance (-  (+ (:principal-balance loan)
                                              (:accrued-interest loan))
                                           (:minimum-monthly-payment loan))
            leftover-extra-payment (- leftover-principal-balance extra-payment)]
        (if (neg? leftover-extra-payment)
          ;; This loan will be fully paid off by the extra payment with extra leftover
          #(bounce-extra-loan-payments
            (abs leftover-extra-payment)
            (concat bounced-loans (list (assoc loan :extra-payment leftover-principal-balance)))
            (rest bouncing-loans))
          ;; Else we have used up all of the extra loan-payment, so mark the amount
          ;; to pay extra on the loan and then return the loans
          (concat bounced-loans (list (assoc loan :extra-payment extra-payment)) (rest bouncing-loans))))))

(defn setup-extra-loan-payments
  "Takes a list of loans and appends extra payment information to them, adhering to the snowball repayment strategy"
  [{:keys [loans extra-loan-payment] :as person}]
  (assoc person :loans
         (trampoline #(bounce-extra-loan-payments extra-loan-payment '() loans))))

(defn process-month
  [person month]
  ;; 
  )

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
        inactive-loan (dissoc (create-loan "inactive-loan" 0.025 99 0 5) :activ)
        loans (list loan inactive-loan)
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
    (total-debt person)
    (sort-by :principal-balance loans)
    loans
    (map #(assoc % :accrued-interest
                 (+ (:accrued-interest %) (* (:principal-balance %) (:interest-rate %))))
         loans)
    (map (fn [{:keys [accrued-interest principal-balance interest-rate] :as loan}]
           (assoc loan :accrued-interest
                  (->> interest-rate
                       (* principal-balance)
                       (+ accrued-interest))))
         loans)
    person
    (accrue-interest loans)
    (process-salary person 1)
    (setup-extra-loan-payments person))


  (def v [1 2 3 4 0 5])

  (reduce (fn [[curr-min min-idx curr-idx] val]
            (if (< val curr-min)
              [val curr-idx (inc curr-idx)]
              [curr-min min-idx (inc curr-idx)]))
          [(first v) 0 0]
          v)
  :rcf)