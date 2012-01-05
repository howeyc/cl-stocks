;;;; scenarios.lisp

(in-package #:cl-stocks)

;;; Helper functions
(defun perform-buy (portfolio stock-price cash-to-use)
  (let ((price (stock-price-close stock-price)) (date (stock-price-date stock-price)))
    (loop 
      for quantity from 50 by 50
      until (<= cash-to-use (* quantity price))
      finally
      (let ((lot (- quantity 50)))
        (if (and (> lot 0)
                 (>= cash-to-use (* lot price)))
          (let ((tran (make-instance 'transaction :date date :type "Buy" :cash (* lot price -1) :quantity lot :price price)))
            (setf (portfolio-avg-price portfolio) (/ (+ (* lot price) (* (portfolio-quantity portfolio) (portfolio-avg-price portfolio))) (+ lot (portfolio-quantity portfolio))))
            (incf (portfolio-quantity portfolio) lot)
            (incf (portfolio-cash portfolio) (* lot price -1))
            (psetf (transaction-accum-cash tran) (portfolio-cash portfolio) (transaction-accum-quantity tran) (portfolio-quantity portfolio))
            (push tran (portfolio-transactions portfolio))))))))

(defun perform-sell (portfolio stock-price force quantity)
  (let* ((price (stock-price-close stock-price)) (proceeds (* quantity price)))
    (if (or force 
         (and (> price (portfolio-avg-price portfolio)) (> proceeds 0)))
      (let ((tran (make-instance 'transaction :date (stock-price-date stock-price) :type "Sell" :cash proceeds :quantity quantity :price price)))
        (setf (portfolio-avg-price portfolio) 0)
        (decf (portfolio-quantity portfolio) quantity)
        (incf (portfolio-cash portfolio) proceeds)
        (psetf (transaction-accum-cash tran) (portfolio-cash portfolio) (transaction-accum-quantity tran) (portfolio-quantity portfolio))
        (push tran (portfolio-transactions portfolio))))))

(defun follow-advice (portfolio current-date stock-price strategy-fn)
 (if (equal current-date (stock-price-date stock-price))
  (multiple-value-bind 
   (advice-string advice-factor force)
   (funcall strategy-fn stock-price)
   (when (string-equal "Buy" advice-string) (perform-buy portfolio stock-price (* advice-factor (portfolio-cash portfolio))))
   (when (string-equal "Sell" advice-string) (perform-sell portfolio stock-price force (* (truncate (* advice-factor (portfolio-quantity portfolio)) 50) 50))))))

(defun collect-div (portfolio dividend)
  (let* ((div (stock-div-div dividend)) (proceeds (* (portfolio-quantity portfolio) div)))
    (if (> proceeds 0)
      (let ((tran (make-instance 'transaction :date (stock-div-date dividend) :type "Div" :cash proceeds :quantity (portfolio-quantity portfolio) :price div :accum-quantity (portfolio-quantity portfolio))))
        (incf (portfolio-cash portfolio) proceeds)
        (setf (transaction-accum-cash tran) (portfolio-cash portfolio))
        (push tran (portfolio-transactions portfolio))))))

(defun collect-split (portfolio stock-split)
  (let* ((split-factor (stock-split-factor stock-split))
         (increase (- (* (portfolio-quantity portfolio) split-factor) (portfolio-quantity portfolio)))
         (tran (make-instance 'transaction :date (stock-split-date stock-split) :type "Split" :quantity increase)))
    (incf (portfolio-quantity portfolio) increase)
    (psetf (transaction-accum-cash tran) (portfolio-cash portfolio) (transaction-accum-quantity tran) (portfolio-quantity portfolio))
    (push tran (portfolio-transactions portfolio))))

(defun run-scenario (portfolio stock-prices dividends splits scenario-fn strategy-fn interest-fn)
  (let ((dividend (pop dividends)) (split (pop splits)) (current-stock-price (pop stock-prices)) (final-price nil))
    (loop for date from (stock-price-date current-stock-price) by 86400
          while current-stock-price do
          (funcall interest-fn portfolio)
          (when (and (not (null dividend)) (>= date (stock-div-date dividend)))
            (collect-div portfolio dividend)
            (setf dividend (pop dividends)))
          (when (and (not (null split)) (>= date (stock-split-date split)))
            (collect-split portfolio split)
            (setf split (pop splits)))
          (setf final-price current-stock-price)
          (funcall scenario-fn portfolio date current-stock-price strategy-fn)
          (when (and (not (null current-stock-price)) (>= date (stock-price-date current-stock-price)))
            (setf current-stock-price (pop stock-prices))))
    (perform-sell portfolio final-price t (portfolio-quantity portfolio))))

(defun run-scenario-with-result (stock-prices dividends splits scenario-fn strategy-fn interest-fn)
 (let ((port (make-instance 'portfolio)))
  (run-scenario port stock-prices dividends splits scenario-fn strategy-fn interest-fn)
  (portfolio-cash port)))

;;; Scenarios
(defun start-investment (portfolio current-date stock-price strategy-fn)
 "Start with 10K only."
 (follow-advice portfolio current-date stock-price strategy-fn))

(defun monthly-investment (portfolio current-date stock-price strategy-fn)
 "Start with 10K, and invest 2K each month."
  (multiple-value-bind
    (secs hour minute day month year)
    (decode-universal-time current-date)
    (declare (ignore secs hour minute month year))
    (if (equal 1 day)
      (incf (portfolio-cash portfolio) 200000000)))
  (follow-advice portfolio current-date stock-price strategy-fn))

(defun monthly-invest-advice (portfolio current-date stock-price strategy-fn)
  "Start with 10K, and invest 2K each month. Only follow advice when deposit money"
  (multiple-value-bind
    (secs hour minute day month year)
    (decode-universal-time current-date)
    (declare (ignore secs hour minute month year))
    (if (equal 1 day)
      (progn
        (incf (portfolio-cash portfolio) 200000000)
        (follow-advice portfolio current-date stock-price strategy-fn)))))

(defun monthly-invest-rule (portfolio current-date stock-price strategy-fn)
  "Start with 10K, and invest 2K each month. Follow % advice from rule when deposit money."
  (multiple-value-bind
    (secs hour minute day month year)
    (decode-universal-time current-date)
    (declare (ignore secs hour minute month year))
    (if (equal 1 day)
      (progn
        (incf (portfolio-cash portfolio) 200000000)
        (perform-buy portfolio stock-price (min (portfolio-cash portfolio) (* (funcall strategy-fn stock-price) (portfolio-cash portfolio))))))))

(defun quarterly-investment (portfolio current-date stock-price strategy-fn)
 "Start with 10K, and invest 6K each quarter."
  (multiple-value-bind
    (secs hour minute day month year)
    (decode-universal-time current-date)
    (declare (ignore secs hour minute year))
    (if (or (and (equal 1 day) (equal 1 month))
            (and (equal 1 day) (equal 4 month))
            (and (equal 1 day) (equal 7 month))
            (and (equal 1 day) (equal 10 month)))
      (incf (portfolio-cash portfolio) 600000000)))
  (follow-advice portfolio current-date stock-price strategy-fn))

(defun yearly-investment (portfolio current-date stock-price strategy-fn)
 "Start with 10K, and invest 24K each year."
  (multiple-value-bind
    (secs hour minute day month year)
    (decode-universal-time current-date)
    (declare (ignore secs hour minute year))
    (if (and (equal 1 day) (equal 1 month))
      (incf (portfolio-cash portfolio) 2400000000)))
  (follow-advice portfolio current-date stock-price strategy-fn))

(defun get-aim-scenario ()
  (let ((portfolio-control 0) (time-to-trade t))
    (defun aim-scenario (portfolio current-date stock-price strategy-fn)
      (declare (ignore strategy-fn))
      (if (zerop portfolio-control)
        (setf portfolio-control 500000000))
      (multiple-value-bind
        (secs hour minute day month year)
        (decode-universal-time current-date)
        (declare (ignore secs hour minute month year))
        (if (equal 15 day)
          (setf time-to-trade t)))
      (if (and time-to-trade (equal current-date (stock-price-date stock-price)))
        (let* ((stock-value (* (portfolio-quantity portfolio) (stock-price-close stock-price)))
              (safe (* 1/10 stock-value))
              (advice (- portfolio-control stock-value))
              (order-value (- (abs advice) safe)))
          (setf time-to-trade nil)
          (if (> order-value (* 50 (stock-price-close stock-price)))
            (progn
              (if (and (> advice 0) (>= (portfolio-cash portfolio) order-value))
                (let ((cash-before-purchase (portfolio-cash portfolio)))
                  (perform-buy portfolio stock-price order-value)
                  (incf portfolio-control (/ (- cash-before-purchase (portfolio-cash portfolio)) 2))))
              (if (< advice 0)
                (perform-sell portfolio stock-price t (* (truncate (* (/ order-value stock-value) (portfolio-quantity portfolio)) 50) 50))))))))))
