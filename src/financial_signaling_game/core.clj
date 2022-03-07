(ns financial-signaling-game.core
 (:require [tech.v3.dataset :as ds]   
           [tech.v3.libs.poi]
           [tech.viz.vega :as vega]))

;; Welcome to the world of signaling game!
(let [x "Amigo"]
  (println (str "Welcome to the world of signaling game, " x "!")))

;; We will have an investment project with payoff R, which attractiveness should be revealed in R>(I(1+r))
(def I (atom (rand 500000) ))
(def rate (atom (rand)))
(def R (atom (+ (* @I (+ 1 @rate)) (rand 500000))))
(def H (atom (+ 50000 (rand 50000))))
(def L (atom (rand 50000)))

;; Under asymmetrical information, different firms will have different offer of equity stakes s.
;; However, this will only happen when firms' payoff of receiving investments are larger than rejecting the offer and doing its own business.
(defn benchmark-firm
"The firms' benchmark of s"
  [P]
(/ @R (+ @R P)))

;; Suppose under asymmetrical information, the investors will have beliefs that q proportion of firms in market has low profitability L.
;; Therefore, the investors will accept the offer if and only if the payoff is larger than leaving its investment elsewhere with interest rate r.
(defn benchmark-investor
  "The firms' benchmark of s"
  [q]
  (/ (* @I (+ 1 @rate)) (+ (* q @L) (* (- 1 q) @H) @R)))

(defn outcome
  [q]
  (cond
    (and (< (benchmark-firm @H) (benchmark-investor q)) (< (benchmark-firm @L) (benchmark-investor q)))
     "Pool-Reject"
    (and (>= (benchmark-firm @H) (benchmark-investor q)) (>= (benchmark-firm @L) (benchmark-investor q)))
    "Pool-Accept"
    (and (< (benchmark-firm @H) (benchmark-investor q)) (>= (benchmark-firm @L) (benchmark-investor q)))
    "Separate-H-Reject"
    :else "Separate-L-Reject"))

;; Now let's incorporate test data.
(def test-data (assoc (ds/->dataset {}) :q (range 0 1 0.005)))
(println (ds/head test-data))

;; Ok good! Then we could use this -main function to detect when there will be pooling or separating equilibriums.
;; Since the low-type firms will imitate high-type firms for higher bonus. The separating equilibrium will emerge when there is one firm rejecting offer.
;; The next step is to make it autonmatically test the reject status under different q R I and lamag.

(defn testrun
  []
  (-> test-data
      (assoc :R @R)
      (assoc :I @I)
      (assoc :H @H)
      (assoc :L @L)
      (assoc :benchmark-investor (for [q (test-data :q)
                                                :let [benchmark-investor (benchmark-investor q)]]
                                            benchmark-investor))
      (assoc :outcome (for [q (test-data :q)
                                     :let [outcome (outcome q)]]
                                 outcome))))

;; According to the model, the separate equiblirum would happen when (benchmark-firm H)<(benchmark-investor 1). 
;; We could use the clojure functions to classify data according to this criterion and see whether there exists separate equilibrium.
(defn eva-cond
  []
  (let [qH  (/ (- (* @I (* (+ 1 @rate) (+ @H @R))) (* @R (+ @H @R))) (* @R (- @L @H))) 
        qL  (/ (- (* @I (* (+ 1 @rate) (+ @L @R))) (* @R (+ @H @R))) (* @R (- @L @H)))]
  (cond
    (true? (< (benchmark-firm @H) (benchmark-investor 1)))
    (do
      (println "(benchmark-firm H)<(benchmark-investor 1)")
      (cond
        (and (and (<= qH 1) (>= qH 0)) (or (> qL 1) (< qL 0)))
        (do 
          (println "Separate-H-Reject")  
          (println qH))
        (and (and (<= qL 1) (>= qL 0)) (or (> qH 1) (< qH 0)))
        (do
          (println "Separate-L-Reject")
          (println qL))
        (and (> qH 1) (> qL 1))
        "Pool-Accept"
        :else "Pool-Reject"))
    :else
    (do
      (println "(benchmark-firm H)>=(benchmark-investor 1)")
      (cond
        (and (and (<= qH 1) (>= qH 0)) (or (> qL 1) (< qL 0)))
        (do
          (println "Separate-H-Reject")
          (println qH))
        (and (and (<= qL 1) (>= qL 0)) (or (> qH 1) (< qH 0)))
        (do
          (println "Separate-L-Reject")
          (println qL))
        (and (> qH 1) (> qL 1))
        "Pool-Accept"
        :else "Pool-Reject")))))
    
;; We could also do data-visualization using tech-viz.
(defn visual
  []
  (as-> (testrun) test-outcome
    (ds/mapseq-reader test-outcome)
    (vega/scatterplot test-outcome :q :benchmark-investor
                      {:title "Equilibrium" 
                       :label-key :outcome
                       :background "white"})
    (vega/vega->svg-file test-outcome "resources/model/visual.svg")))

(defn -main
  []
  (reset! rate (rand))
  (reset! I (rand 500000))
  (reset! R (+ (* @I (+ 1 @rate)) (rand 500000)))
  (println (ds/head (testrun)))
  (ds/write! (testrun) "resources/model/test-outcome.csv")
  (println (eva-cond))
  (visual))
