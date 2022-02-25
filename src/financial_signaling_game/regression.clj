(ns financial-signaling-game.regression
(:require
 [financial-signaling-game.data :refer [raw-data processing]]
 [tech.v3.dataset :as ds]
 [tech.v3.libs.poi]
 [tech.v3.libs.xgboost]
 [tech.v3.dataset.math :as ds-math]
 [tech.v3.dataset.column-filters :as cf]
 [tech.v3.ml :as ml]
 [tech.v3.ml.loss :as loss]
 [tech.v3.ml.metrics :as metrics]
 [tech.v3.ml.gridsearch :as gs]
 [tech.v3.dataset.modelling :as ds-mod])
(:use (incanter core optimize stats datasets charts io)))
;; Based on the property in the proposed model, we could find that the equilibrium share ratio will increased upon the decrease of investors' investment confidence.
;; With higher investment confidence, there are also higher probabilities for higher investment amount and more investors engaged in. 
;; Therefore, we could use statistical regression to see whether share ratio will re()ally have signaling effects and how significant these impacts are. 
;; For comparing significance level of contribution, I could use tech.ml to have a regression model (ml/explain).
;; For statistical tools, I could also use incanter to conduct t-test and f-test.

;; Import the dataset
(def real-data (processing raw-data))
;; We could check the status of all data collected since we only need the data with integers or float numbers for regression.
(defn check
  []
  (->> (map (comp :datatype meta)
            (vals real-data))
       (frequencies)))

;; Before the regression work, we could firstly check the correlation among each variables to have a preliminary evaluation.
(defn corr-matrix
"Generating a correlation matrix of all variables"  
  []
  (-> (ds/->dataset (ds-math/correlation-table 
                    real-data 
                    :colname-seq [:Age
                                  :Duration
                                  :Share_Ratio
                                  :Investor_Equity_Total
                                  :Round_Equity_Total
                                  :Company_Status
                                  :Num_Investors
                                  :lSuccess
                                  :lNum_Investors]))
    (ds/write! "resources/correlation-matrix.csv")))

;; Now let's do the regression work! We will test the signaling effects of equity share ratio to the number of investors and investment amount.
;; Now let's do the regression work! We will test the signaling effects of equity share ratio to the number of investors and investment amount.
;; We should firstly set the inference target.
;; We could split the datasets to "train" and "test". Then we could train one and test another.
(def reg-investamt-ds (-> real-data
                          (dissoc :Num_Investors :lNum_Investors :Success)
                          (ds/categorical->number cf/categorical)))
(def reg-investamt (ds-mod/set-inference-target reg-investamt-ds :lSuccess))
(def split-amt (ds-mod/train-test-split reg-investamt))
(def model-amt (ml/train (:train-ds split-amt) {:model-type :xgboost/regression}))
(def predict-amt (ml/predict (:test-ds split-amt) model-amt))
(def mse-amt (loss/mse (predict-amt :lSuccess)
                       ((:test-ds split-amt) :lSuccess)))

;; Then we could make a prediction of investment amount using our test-ds with the reference to regression training outcomes.
;; By using the loss function, we could have a check on the residuals of each indepvar in regression.
(defn outcome-amt
  []
  (-> (ml/explain model-amt) 
      (assoc :loss mse-amt)
      (ds/write! "resources/outcome-amt.csv")))

;; We could also have regression tests on Number of investors
(def reg-num-investor-ds (-> real-data
                             (dissoc :Num_Investors :lSuccess :Success)
                             (ds/categorical->number cf/categorical)))
(def reg-num-investor (ds-mod/set-inference-target reg-num-investor-ds :lNum_Investors))
(def split-num (ds-mod/train-test-split reg-num-investor))
(def model-num (ml/train (:train-ds split-num) {:model-type :xgboost/regression}))
(def predict-num (ml/predict (:test-ds split-num) model-num))
(def mse-num (metrics/precision ((:test-ds split-num) :lNum_Investors) (predict-num :lNum_Investors)))
(defn outcome-num
  []
  (-> (ml/explain model-num)
      (assoc :precision mse-num)
      (ds/write! "resources/outcome-num.csv")))

;; It seems that our regression did not have a decent outcome.
;; We could make our estimation more precise by using the "gridsearch" function in tech.ml
(def gridsearchable-options (merge {:model-type :xgboost/regression} (ml/hyperparameters :xgboost/regression)))
(def option-seq (take 100 (gs/sobol-gridsearch gridsearchable-options)))

(defn test-options-amt
  [options]
  (let [model (ml/train (:train-ds split-amt) options)
        predict-amt (ml/predict (:test-ds split-amt) model-amt)]
    (assoc model :loss (loss/mse (predict-amt :lSuccess)
                                 ((:test-ds split-amt) :lSuccess)))))
(def search-results-amt
  (->> option-seq
       (map test-options-amt)
       (sort-by :loss)
       (take 10)))

;; In tech.ml, the xgboost library has a huge archive of machine-learning methods. We could train the model with the best options.
(def model-options (:options (first search-results-amt)))
(def new-model-amt (ml/train (:train-ds split-amt) model-options))
(def new-predict-amt (ml/predict (:test-ds split-amt) new-model-amt))
(def new-mse-amt (loss/mse (new-predict-amt :lSuccess)
                           ((:test-ds split-amt) :lSuccess)))
(def new-precision-amt (metrics/accuracy ((:test-ds split-amt) :lSuccess) (new-predict-amt :lSuccess)))

;; We could also do that on the test of investor-numbers
(defn test-options-num
  [options]
  (let [model (ml/train (:train-ds split-num) options)
        predict-num (ml/predict (:test-ds split-num) model-num)]
    (assoc model :loss (loss/mse (predict-num :lNum_Investors)
                                 ((:test-ds split-num) :lNum_Investors)))))
(def search-results-num
  (->> option-seq
       (map test-options-num)
       (sort-by :loss)
       (take 10)))

(def model-options-num (:options (first search-results-num)))
(def new-model-num (ml/train (:train-ds split-num) model-options-num))
(def new-predict-num (ml/predict (:test-ds split-num) new-model-num))
(def new-mse-num (loss/mse (new-predict-num :lNum_Investors)
                           ((:test-ds split-num) :lNum_Investors)))
(def new-precision-num (metrics/accuracy ((:test-ds split-num) :lNum_Investors) (new-predict-num :lNum_Investors)))

(defn check-precision
  "Check whether there are some better approaches for machine learning"
  []
  (println new-mse-amt)
  (println new-mse-num)
  (println new-precision-amt)
  (println new-precision-num))

;; We could have some statistical results about our regression using incanter.
;; Let's firstly import our dataset.
(def reg-data (ds/value-reader real-data))
(def amt-investment (sel reg-data :cols 12))
(def num-investor (sel reg-data :cols 13))
(def x (sel reg-data :cols '(0 1 2 3 5 6 7 8 9 10)))

(defn reg-beta
  []
  #_{:clj-kondo/ignore [:redundant-do]}
  (do 
  (println (ds/->dataset (simple-regression amt-investment x)))
  (println (ds/->dataset (simple-regression num-investor x)))))

(defn stat-amt
  "print out the regression outcome of investment amount"
    []  
    (-> (ds/->dataset (linear-model amt-investment x))
        (dissoc :coef-var :coefs-ci :design-matrix :x)
        (ds/write! "resources/stat-amt.csv")))

(defn stat-num
  "print out the regression outcome of investor numbers"
  []
  (-> (ds/->dataset (linear-model num-investor x))
      (dissoc :coef-var :coefs-ci :design-matrix :x)
      (ds/write! "resources/stat-num.csv")))

(defn outcome
  []
  (outcome-amt) (outcome-num) (check-precision))

(defn stat
  []
  (reg-beta) (stat-amt) (stat-num))