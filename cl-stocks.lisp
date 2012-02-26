;;;; cl-stocks.lisp

(in-package #:cl-stocks)

(defun run-all-scenarios (ticker start end out-stream)
  (let ((interest-fn (get-daily-interest-rate (/ 1 100)))
        (scenario-result 0))
    (multiple-value-bind (stock-prices dividends splits)
      (get-stock-info ticker start end)
      (loop for scenario-fn in (list (get-aim-scenario)) do 
            (setf scenario-result (run-scenario-with-result stock-prices dividends splits scenario-fn nil interest-fn))
            (format out-stream "Scenario: ~21A Strategy: ~19A Result: ~10,2F~%" scenario-fn nil scenario-result))
      (format out-stream "~%")
      (loop for scenario-fn in '(start-investment monthly-investment quarterly-investment yearly-investment) do 
            (loop for strategy-fn in (append '(in-the-bank buy-when-possible thirty-percent-rule sma-rule moneypaper-invest drip-invest-calc) (list (get-twinvest-fn))) do
                  (multiple-value-bind (scenario-result transactions)
                    (run-scenario-with-result stock-prices dividends splits scenario-fn strategy-fn interest-fn)
                    (format out-stream "Scenario: ~21A Strategy: ~19A Result: ~10,2F~%" scenario-fn strategy-fn scenario-result)))
            (format out-stream "~%")))))

(defun run-portfolios (tickers start end out-stream)
  (let ((interest-fn (get-daily-interest-rate (/ 1 100)))
        (ticker-hash (get-ticker-hash tickers start end))
        (portfolio-result 0))
    (loop for pick-fn in '(pick-max pick-avg pick-wavg) do
          (loop for strategy-fn in '(moneypaper-invest drip-invest-calc) do
                (multiple-value-bind (portfolio-result transactions)
                  (run-a-portfolio ticker-hash tickers start end interest-fn strategy-fn pick-fn)
                  (format out-stream "Pick: ~21A Strategy: ~19A Result: ~10,2F~%" pick-fn strategy-fn portfolio-result)))
          (format out-stream "~%"))))
