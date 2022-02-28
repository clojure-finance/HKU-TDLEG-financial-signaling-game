(ns financial-signaling-game.data
(:require 
 [tech.v3.dataset :as ds]
 [tech.v3.libs.poi]
 [tech.v3.datatype.datetime :as dtype-dt]
 [tech.v3.datatype.functional :as dfn]
 [tech.v3.dataset.column-filters :as cf]
 [tech.v3.dataset.reductions :as ds-reduce]
 [tech.viz.vega :as vega])
(:use (incanter core stats datasets charts io)))

;; Then we finish our experimentation of our model. The next step is incorporate real data from database.
;; The first thing to do is to import the database and make it clean.
;; There are some missing values, zeros with no research values, categorical variables, values in different years etc. 
;; We should resolve these issue, and incorporate the real independent variabe: the share ratio.
;; After the cleaning work, we could also have a description of our collected data.

;; Import the database
(def raw-data (ds/->dataset "resources/US-data.csv"  {:key-fn keyword}))
(defn importdata 
  "Import the database, trim the zero value data, and sorty the data by the investments' stage (seed or early)"  
  [ds]
  (-> ds 
      (ds/drop-missing)    
      (ds/select-columns [:Round
                          :Industry_Sector
                          :Investment_Date
                          :Investor_Equity_Total
                          :Round_Equity_Total
                          :Disclosed_Equity_Contribution
                          :Num_Investors
                          :Company_Status
                          :Duration
                          :Location
                          :Age]) 
      (ds/filter #(> (get % :Round_Equity_Total) 0))
      (ds/filter #(> (get % :Disclosed_Equity_Contribution) 0))
      (ds/filter #(> (get % :Num_Investors) 0))
      (ds/sort-by-column :Company_Status)))

(defn choosedate
  "Select the data over the past 10 years."  
  [ds]
  (-> ds
      (assoc :Investment_Date_Code (dtype-dt/datetime->milliseconds (ds :Investment_Date)))
      (assoc :years (dtype-dt/long-temporal-field :years (ds :Investment_Date)))
      (ds/filter #(<= 2012 (get % :years)))))

(defn indepvar 
  "Calculate the Independent Variable"  
  [ds]
  (-> ds
      (ds/column-map :Share_Ratio (fn [Investor_Equity_Total Round_Equity_Total]
                                    (when (and Investor_Equity_Total Round_Equity_Total)
                                      (/ (double Investor_Equity_Total) (double Round_Equity_Total))))
                     [:Investor_Equity_Total :Round_Equity_Total])
      (ds/filter #(>= (get % :Share_Ratio) 0))
      (ds/filter #(<= (get % :Share_Ratio) 1))))

(defn depvar
  "Retrieve logarithm value of Share_Ratio, Num_Investors, and Investment Amount. Filtering the Value less than 1"
  [ds]
  (as-> ds real-data
      (assoc real-data :Success (real-data :Disclosed_Equity_Contribution)) 
      (assoc real-data :lSuccess (dfn/log (real-data :Success)))
      (assoc real-data :lNum_Investors (dfn/log (real-data :Num_Investors)))))

(defn category-location
  [n]
  (cond
    (= n "New York")
    "New York"
    :else
    "Others"))

(defn controlvar
"Convert Location into a binary dummy variable: New York or Others."  
  [ds]  
  (as-> ds real-data
    (assoc real-data :Location (for [location (real-data :Location)
                                     :let [catlocation (category-location location)]]
                                 catlocation))))

(defn importregdata
"Only keep columns with numbers for regression."  
  [ds]
  (-> ds
      (dissoc 
       :Investment_Date
       :Investment_Date_Code
       :Disclosed_Equity_Contribution)
       (ds/categorical->number cf/categorical)))

(defn processing
"Processing data and prepare a discription"  
  [ds]
  (-> ds
      (importdata)
      (choosedate)
      (indepvar)
      (controlvar)
      (depvar)
      (importregdata)))

(defn description 
  "Give a brief description about data."
  [ds] 
  (as-> ds desds
    (println (ds/head (ds/descriptive-stats desds))
    (ds/write! (ds/descriptive-stats desds) "resources/data-description.csv"))))

(defn visual-amt
"Visualizing relationship between share ratio and investor numbers"  
  [ds]
  (-> (-> ds (importdata) (choosedate) (indepvar) (controlvar) (depvar))
      (ds/mapseq-reader)
      (vega/scatterplot :Share_Ratio :lSuccess
                        {:title "Relationship-amt"
                         :label-key :Company_Status
                         :background "white"})
      (vega/vega->svg-file "resources/visual-reg-amt.svg")))

(defn visual-num
"Visualizing relationship between share ratio and investor numbers"  
  [ds]
  (-> (-> ds (importdata) (choosedate) (indepvar) (controlvar) (depvar))
      (ds/mapseq-reader)
      (vega/scatterplot :Share_Ratio :Num_Investors
                        {:title "Relationship-num"
                         :label-key :Company_Status
                         :background "white"})
      (vega/vega->svg-file "resources/visual-reg-num.svg")))

(defn visual-control
"Produce pie charts depicting investee companies' status using incanter"  
  [ds]  
  (with-data (->> (-> ds (importdata) (choosedate) (indepvar) (controlvar) (depvar) (ds/mapseq-reader) (seq) (to-dataset))
                 ($rollup :count :Share_Ratio :Location))
    (save (pie-chart :Location :Share_Ratio :title "Location Distribution") "resources/visual-location.png"))
  (with-data (->> (-> ds (importdata) (choosedate) (indepvar) (controlvar) (depvar) (ds/mapseq-reader) (seq) (to-dataset))
                  ($rollup :count :Share_Ratio :Company_Status))
    (save (pie-chart :Company_Status :Share_Ratio :title "Company Status Distribution") "resources/visual-company-status.png"))
  (with-data (->> (-> ds (importdata) (choosedate) (indepvar) (controlvar) (depvar) (ds/mapseq-reader) (seq) (to-dataset))
                  ($rollup :count :Share_Ratio :Industry_Sector))
    (save (pie-chart :Industry_Sector :Share_Ratio :title "Industry Distribution") "resources/visual-industry.png")))

(defn category-invest-amt
  "group investor number into 5 categories for visualization"
  [amt]
  (cond
    (and (>= amt 0) (< amt 25000000))
    "0-0025"
    (and (>= amt 25000000) (< amt 50000000))
    "0-0050"
    (and (>= amt 50000000) (< amt 100000000))
    "0050-0100"
    (and (>= amt 100000000) (< amt 200000000))
    "0100-0200"
    (and (>= amt 200000000) (< amt 400000000))
    "0200-0400"
    (and (>= amt 400000000) (< amt 800000000))
    "0400-0800"
    (and (>= amt 800000000) (< amt 1200000000))
    "0800-1200"
    :else
    "1200+"))

(defn pivot-table
"Depicting the share ratio and amount in different firms, producing an excel-pivot-table-like dataset."  
  [ds]
  (as-> ds visds
    (-> visds (importdata) (choosedate) (indepvar) (controlvar) (depvar))
    (ds-reduce/group-by-column-agg
     :Company_Status
     {:Company_Status (ds-reduce/first-value :Company_Status)
      :Share_Ratio_avg (ds-reduce/mean :Share_Ratio)
      :Invest_Amount_avg (ds-reduce/mean :Success) 
      :Num_Investors_avg (ds-reduce/mean :Num_Investors)}
     visds)
    (ds/sort-by-column visds :Company_Status)))

(defn visual-pivot-table
  "Visualizing the relationship between company status and share ratio."
  [ds]
  (with-data (-> ds
                 (pivot-table)
                 (ds/mapseq-reader)
                 (seq)
                 (to-dataset))
    (save (stacked-bar-chart :Company_Status :Share_Ratio_avg
                             :title "Share_Ratio_Status_Relationships"
                             :group-by :Company_Status
                             :x-label "Company Status"
                             :y-label "Average Share Ratio"
                             :legend true)
          "resources/visual-pivot-table.png")))

(defn visual-relationship-data
  [ds]
  (as-> ds visds
    (assoc visds :Invest_Amount (for [Success (visds :Success)
                                      :let [Invest_Amount (category-invest-amt Success)]]
                                  Invest_Amount))
    (ds-reduce/group-by-column-agg
     :Invest_Amount
     {:Invest_Amount (ds-reduce/first-value :Invest_Amount)
      :Num_Investors_avg (ds-reduce/mean :Num_Investors)
      :Number (ds-reduce/row-count)}
     visds)
    (ds/sort-by-column visds :Invest_Amount)))

(defn visual-relationship
"Visualizing the relationship between investor number and fund amount."  
  [ds]
  (with-data (-> ds
               (visual-relationship-data)
               (ds/mapseq-reader)
               (seq)
               (to-dataset))
    (save (stacked-bar-chart :Invest_Amount :Num_Investors_avg               
                     :title "Investment_Investor_Relationships"
                     :group-by :Invest_Amount
                     :x-label "Investment Amount" 
                     :y-label "Average Investment Number"
                     :legend true)
        "resources/visual-relationship.png")))

(defn visualize
  "Visualize the selected data."
  []
  (description (processing raw-data))
  (visual-amt raw-data)
  (visual-num raw-data)
  (visual-control raw-data)
  (ds/write! (pivot-table raw-data) "resources/pivot-table.csv")
  (visual-pivot-table raw-data)
  (visual-relationship (processing raw-data))
  (ds/write! (processing raw-data) "resources/regression-data.csv"))
