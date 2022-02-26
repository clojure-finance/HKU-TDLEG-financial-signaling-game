(ns financial-signaling-game.robust
(:require
 [financial-signaling-game.data :refer [category-invest-amt visualize]]
 [financial-signaling-game.regression :refer [real-data check corr-matrix outcome stat]] 
 [tech.v3.dataset :as ds]
 [tech.v3.libs.poi]
 [tech.v3.libs.xgboost]
 [tech.v3.dataset.column-filters :as cf]
 [tech.v3.ml :as ml]
 [tech.v3.ml.metrics :as metrics]
 [tech.v3.dataset.modelling :as ds-mod]
 [clojisr.v1.r :as r :refer :all]))

;; We would like to design a robustness-check by using the logistic regression of investment success.
(def robust-amt-ds (-> (dissoc real-data :Num_Investors :lNum_Investors :lSuccess :Company_Status)
                       (assoc :Invest_Amt (for [Invest_amt (real-data :Success)
                                                            :let [Invest_amt_Category (category-invest-amt Invest_amt)]]
                                                        Invest_amt_Category))
                       (ds/sort-by-column :Invest_Amt)))
(def reg-success (-> (dissoc robust-amt-ds :Success)
                             (ds/categorical->number cf/categorical)
                             (ds-mod/set-inference-target :Invest_Amt)))
(def split-success (ds-mod/train-test-split reg-success))
(def model-success (ml/train (:train-ds split-success) {:model-type :xgboost/classification}))
(def predict-success (ml/predict (:test-ds split-success) model-success))
(def precision-success (metrics/precision ((:test-ds split-success) :Invest_Amt) (predict-success :Invest_Amt)))

;; We could firstly use the logistic regression methods to test investment amount using the categories we created in financial-signaling-game.data..
(defn robust-outcome-amt
  []
  (-> (ml/explain model-success)
      (assoc :precision precision-success)
      (ds/write! "resources/robust-outcome-amt.csv")))

;; As a robustness check, we could use company current status as dependent variable, which would also be a pertinent measure of investment success.
;; Here, we could use cox-regression
(def robust-status-ds (-> (dissoc real-data :Num_Investors :lNum_Investors :lSuccess :Success)))
(def reg-status (-> robust-status-ds
                     (ds/categorical->number cf/categorical)
                     (ds-mod/set-inference-target :Company_Status)))
(def split-status (ds-mod/train-test-split reg-status))
(def model-status (ml/train (:train-ds split-status) {:model-type :xgboost/survival-cox}))
(def predict-status (ml/predict (:test-ds split-status) model-status))
(def precision-status (metrics/precision ((:test-ds split-status) :Company_Status) (predict-status :Company_Status)))
(defn robust-outcome-status
  []
  (-> (ml/explain model-status)
      (assoc :precision precision-status)
      (ds/write! "resources/robust-outcome-status.csv")))

;; We could also do robustness check on investor number with a poisson regression
(def robust-num-ds (dissoc real-data :lNum_Investors :Success :lSuccess :Company_Status))
(def robust-num-investor (-> robust-num-ds
                             (ds/categorical->number cf/categorical)
                             (ds-mod/set-inference-target :Num_Investors)))
(def robust-split-num (ds-mod/train-test-split robust-num-investor))
(def robust-model-num (ml/train (:train-ds robust-split-num) {:model-type :xgboost/count-poisson}))
(def robust-predict-num (ml/predict (:test-ds robust-split-num) robust-model-num))
(def precision-num (metrics/precision ((:test-ds robust-split-num) :Num_Investors) (robust-predict-num :Num_Investors)))

(defn robust-outcome-num
  []
  (-> (ml/explain robust-model-num)
      (assoc :precision precision-num)
      (ds/write! "resources/robust-outcome-num.csv")))

(defn stat-robust-num
  []  
  (let [glm (r '(function [ds] (print (summary (glm (formula Num_Investors
                                                      (+ Round
                                                         Industry_Sector
                                                         Investor_Equity_Total
                                                         Round_Equity_Total
                                                         Duration
                                                         Location
                                                         Age
                                                         years
                                                         Share_Ratio))
                                             :data ds 
                                             :family poisson)))))]
    (glm real-data)))

(defn robust
  []
  (robust-outcome-amt)
  (robust-outcome-status)
  (robust-outcome-num)
  (stat-robust-num))

(defn -main
  []
  (check)
  (visualize)
  (corr-matrix)  
  (outcome) 
  (stat) 
  (robust))