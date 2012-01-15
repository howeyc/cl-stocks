;;;; cl-stocks.lisp

(in-package #:cl-stocks)

(defun run-them-all (file-prices file-divs file-splits out-stream)
  (let ((stock-prices (parse-stock-prices file-prices))
        (dividends (parse-stock-divs file-divs))
        (splits (parse-stock-splits file-splits))
        (interest-fn (get-daily-interest-rate-accumulator (/ 1 100)))
        (scenario-result 0))
    (loop for scenario-fn in (list (get-aim-scenario)) do 
          (setf scenario-result (run-scenario-with-result stock-prices dividends splits scenario-fn nil interest-fn))
          (format out-stream "Scenario: ~21A Strategy: ~19A Result: ~10,2F~%" scenario-fn nil scenario-result))
    (format out-stream "~%")
    (loop for scenario-fn in '(start-investment monthly-investment monthly-invest-advice quarterly-investment yearly-investment) do 
          (loop for strategy-fn in '(in-the-bank buy-when-possible thirty-percent-rule) do
                (setf scenario-result (run-scenario-with-result stock-prices dividends splits scenario-fn strategy-fn interest-fn))
                (format out-stream "Scenario: ~21A Strategy: ~19A Result: ~10,2F~%" scenario-fn strategy-fn scenario-result))
          (format out-stream "~%"))
    (loop for monthly-invest-rule-fn in (append '(moneypaper-invest drip-invest-calc) (list (get-twinvest-fn))) do 
          (setf scenario-result (run-scenario-with-result stock-prices dividends splits 'monthly-invest-rule monthly-invest-rule-fn interest-fn))
          (format out-stream "Scenario: ~21A Strategy: ~19A Result: ~10,2F~%" 'monthly-invest-rule monthly-invest-rule-fn scenario-result))))

(defun run-yahoo-data (ticker start end out-stream)
  (let ((interest-fn (get-daily-interest-rate-accumulator (/ 1 100)))
        (scenario-result 0))
    (multiple-value-bind (stock-prices dividends splits)
      (get-stock-info ticker start end)
      (loop for scenario-fn in (list (get-aim-scenario)) do 
            (setf scenario-result (run-scenario-with-result stock-prices dividends splits scenario-fn nil interest-fn))
            (format out-stream "Scenario: ~21A Strategy: ~19A Result: ~10,2F~%" scenario-fn nil scenario-result))
      (format out-stream "~%")
      (loop for scenario-fn in '(start-investment monthly-investment monthly-invest-advice quarterly-investment yearly-investment) do 
            (loop for strategy-fn in '(in-the-bank buy-when-possible thirty-percent-rule) do
                  (setf scenario-result (run-scenario-with-result stock-prices dividends splits scenario-fn strategy-fn interest-fn))
                  (format out-stream "Scenario: ~21A Strategy: ~19A Result: ~10,2F~%" scenario-fn strategy-fn scenario-result))
            (format out-stream "~%"))
      (loop for monthly-invest-rule-fn in (append '(moneypaper-invest drip-invest-calc) (list (get-twinvest-fn))) do 
            (setf scenario-result (run-scenario-with-result stock-prices dividends splits 'monthly-invest-rule monthly-invest-rule-fn interest-fn))
            (format out-stream "Scenario: ~21A Strategy: ~19A Result: ~10,2F~%" 'monthly-invest-rule monthly-invest-rule-fn scenario-result)))))