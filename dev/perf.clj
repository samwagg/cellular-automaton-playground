(ns perf
  (:require
   [criterium.core :as criterium]
   [ca-playground.ca :as ca]
   [ca-playground.grid :as grid]))

(defn bench-gol
  [grid-rows grid-cols n-updates]
  (println "---------------------------")
  (println (format "Benchmarking Game of Life CA with %s X %s grid, %s updates." grid-rows grid-cols n-updates))
  (let [init-grid (grid/vec-grid-of (repeatedly #(rand-int 2)) grid-rows grid-cols)]
      (criterium/bench
        (->> init-grid
             (iterate #(ca/update-grid % ca/game-of-life-update-fn))
             (take n-updates)
             dorun)))
  (println "---------------------------"))

(comment
  (run! #(apply bench-gol %)
        [[10 10 100]
         [50 50 100]
         [100 100 100]])

;; ---------------------------
;; Benchmarking Game of Life CA with 10 X 10 grid, 100 updates.
;; Evaluation count : 1500 in 60 samples of 25 calls.
;;              Execution time mean : 40.697988 ms
;;     Execution time std-deviation : 719.141063 µs
;;    Execution time lower quantile : 39.928879 ms ( 2.5%)
;;    Execution time upper quantile : 41.802474 ms (97.5%)
;;                    Overhead used : 6.459185 ns

;; Found 2 outliers in 60 samples (3.3333 %)
;; 	low-severe	 1 (1.6667 %)
;; 	low-mild	 1 (1.6667 %)
;;  Variance from outliers : 6.2960 % Variance is slightly inflated by outliers
;; ---------------------------
;; ---------------------------
;; Benchmarking Game of Life CA with 50 X 50 grid, 100 updates.
;; Evaluation count : 60 in 60 samples of 1 calls.
;;              Execution time mean : 1.061175 sec
;;     Execution time std-deviation : 24.298947 ms
;;    Execution time lower quantile : 1.043904 sec ( 2.5%)
;;    Execution time upper quantile : 1.131052 sec (97.5%)
;;                    Overhead used : 6.459185 ns

;; Found 4 outliers in 60 samples (6.6667 %)
;; 	low-severe	 1 (1.6667 %)
;; 	low-mild	 3 (5.0000 %)
;;  Variance from outliers : 10.9906 % Variance is moderately inflated by outliers
;; ---------------------------
;; ---------------------------
;; Benchmarking Game of Life CA with 100 X 100 grid, 100 updates.
;; Evaluation count : 60 in 60 samples of 1 calls.
;;              Execution time mean : 4.256438 sec
;;     Execution time std-deviation : 30.105514 ms
;;    Execution time lower quantile : 4.188672 sec ( 2.5%)
;;    Execution time upper quantile : 4.308722 sec (97.5%)
;;                    Overhead used : 6.459185 ns

;; Found 2 outliers in 60 samples (3.3333 %)
;; 	low-severe	 1 (1.6667 %)
;; 	low-mild	 1 (1.6667 %)
;;  Variance from outliers : 1.6389 % Variance is slightly inflated by outliers
;;  ---------------------------
 )
