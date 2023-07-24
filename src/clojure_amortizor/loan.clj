(ns clojure-amortizor.loan)

(defn create-loan
  "Creates an active loan"
  [name interest-rate principal-balance accrued-interest minimum-monthly-payment]
  {:name name
   :interest-rate interest-rate
   :principal-balance principal-balance
   :accrued-interest accrued-interest
   :minimum-monthly-payment minimum-monthly-payment
   :active true})

(defn calculate-daily-interest
  [{:keys [interest-rate principal-balance] :as loan}]
  (/ (* principal-balance interest-rate) 365.25))

(defn get-accrued-interest
  [days daily-interest]
  (* days daily-interest))
