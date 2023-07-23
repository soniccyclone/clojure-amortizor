(ns clojure-amortizor.person)

(defn create-person
  [extra-loan-payment loans annual-salary annual-raise-month annual-raise-percent percent-of-raise-for-repayment]
  {:extra-loan-payment extra-loan-payment
   :loans loans
   :annual-salary annual-salary
   :annual-raise-month annual-raise-month
   :annual-raise-percent annual-raise-percent
   :percent-of-raise-for-repayment percent-of-raise-for-repayment})

(defn applicable-loans
  [{:keys [loans] :as person}]
  (filter #(:active %) loans))

(defn total-debt
  [person]
  (->> (applicable-loans person)
       (map :principal-balance)
       (reduce +)))

(defn find-loan-for-extra-payment
  "DON'T USE THIS!! just order the loans in increasing order of principal balance and then trampoline down that attaching an :extra-payment to each loan until you have run out of the extra monthly payment"
  [loans]
  (reduce #(if (< (:principal-balance %2) (:principal-balance %1)) %2 %1)
          loans))