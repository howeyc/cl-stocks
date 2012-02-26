;;;; scenarios.lisp

(in-package #:cl-stocks)

;;; Helper functions
(defun perform-buy (stock-position stock-price cash-to-use)
  (let ((price (stock-price-close stock-price)) (date (stock-price-date stock-price)))
    (loop 
      for quantity from 50 by 50
      until (<= cash-to-use (* quantity price))
      finally
      (let ((lot (- quantity 50)))
        (if (and (> lot 0)
                 (>= cash-to-use (* lot price)))
          (let ((tran (make-instance 'transaction :date date :type "Buy" :cash (* lot price -1) :quantity lot :price price)))
            (setf (stock-position-avg-price stock-position) (/ (+ (* lot price) (* (stock-position-quantity stock-position) (stock-position-avg-price stock-position))) (+ lot (stock-position-quantity stock-position))))
            (incf (stock-position-quantity stock-position) lot)
            (incf (stock-position-cash stock-position) (* lot price -1))
            (psetf (transaction-accum-cash tran) (stock-position-cash stock-position) (transaction-accum-quantity tran) (stock-position-quantity stock-position))
            (push tran (stock-position-transactions stock-position))))))))

(defun perform-sell (stock-position stock-price force quantity)
  (let* ((price (stock-price-close stock-price)) (proceeds (* quantity price)))
    (if (or force 
         (and (> price (stock-position-avg-price stock-position)) (> proceeds 0)))
      (let ((tran (make-instance 'transaction :date (stock-price-date stock-price) :type "Sell" :cash proceeds :quantity quantity :price price)))
        (setf (stock-position-avg-price stock-position) 0)
        (decf (stock-position-quantity stock-position) quantity)
        (incf (stock-position-cash stock-position) proceeds)
        (psetf (transaction-accum-cash tran) (stock-position-cash stock-position) (transaction-accum-quantity tran) (stock-position-quantity stock-position))
        (push tran (stock-position-transactions stock-position))))))

(defun follow-advice (stock-position current-date stock-price strategy-fn)
 (if (equal current-date (stock-price-date stock-price))
  (multiple-value-bind 
   (advice-string advice-factor force)
   (funcall strategy-fn stock-price)
   (when (string-equal "Buy" advice-string) (perform-buy stock-position stock-price (min (stock-position-cash stock-position) (* advice-factor (stock-position-cash stock-position)))))
   (when (string-equal "Sell" advice-string) (perform-sell stock-position stock-price force (* (truncate (* advice-factor (stock-position-quantity stock-position)) 50) 50))))))

(defun collect-div (stock-position dividend)
  (let* ((div (stock-div-div dividend)) (proceeds (* (stock-position-quantity stock-position) div)))
    (if (> proceeds 0)
      (let ((tran (make-instance 'transaction :date (stock-div-date dividend) :type "Div" :cash proceeds :quantity (stock-position-quantity stock-position) :price div :accum-quantity (stock-position-quantity stock-position))))
        (incf (stock-position-cash stock-position) proceeds)
        (setf (transaction-accum-cash tran) (stock-position-cash stock-position))
        (push tran (stock-position-transactions stock-position))))))

(defun collect-split (stock-position stock-split)
  (let* ((split-factor (stock-split-factor stock-split))
         (increase (- (* (stock-position-quantity stock-position) split-factor) (stock-position-quantity stock-position)))
         (tran (make-instance 'transaction :date (stock-split-date stock-split) :type "Split" :quantity increase)))
    (incf (stock-position-quantity stock-position) increase)
    (psetf (transaction-accum-cash tran) (stock-position-cash stock-position) (transaction-accum-quantity tran) (stock-position-quantity stock-position))
    (push tran (stock-position-transactions stock-position))))

(defun run-scenario (stock-position stock-prices dividends splits scenario-fn strategy-fn interest-fn)
  (let ((dividend (pop dividends)) (split (pop splits)) (current-stock-price (pop stock-prices)) (final-price nil))
    (loop for date from (stock-price-date current-stock-price) by 86400
          while current-stock-price do
          (incf (stock-position-cash stock-position) (funcall interest-fn (stock-position-cash stock-position)))
          (when (and (not (null dividend)) (>= date (stock-div-date dividend)))
            (collect-div stock-position dividend)
            (setf dividend (pop dividends)))
          (when (and (not (null split)) (>= date (stock-split-date split)))
            (collect-split stock-position split)
            (setf split (pop splits)))
          (setf final-price current-stock-price)
          (funcall scenario-fn stock-position date current-stock-price strategy-fn)
          (when (and (not (null current-stock-price)) (>= date (stock-price-date current-stock-price)))
            (setf current-stock-price (pop stock-prices))))
    (perform-sell stock-position final-price t (stock-position-quantity stock-position))))

(defun run-scenario-with-result (stock-prices dividends splits scenario-fn strategy-fn interest-fn)
  (let ((spos (make-instance 'stock-position)))
    (run-scenario spos stock-prices dividends splits scenario-fn strategy-fn interest-fn)
    (values 
      (stock-position-cash spos)
      (stock-position-transactions spos))))

;;; Scenarios
(defun start-investment (stock-position current-date stock-price strategy-fn)
 "Start with 10K only."
 (follow-advice stock-position current-date stock-price strategy-fn))

(defun monthly-investment (stock-position current-date stock-price strategy-fn)
  "Start with 10K, and invest 2K each month."
  (multiple-value-bind
    (secs hour minute day month year)
    (decode-universal-time current-date)
    (declare (ignore secs hour minute month year))
    (when (equal 1 day)
      (incf (stock-position-cash stock-position) 2000)))
  (follow-advice stock-position current-date stock-price strategy-fn))

(defun quarterly-investment (stock-position current-date stock-price strategy-fn)
  "Start with 10K, and invest 6K each quarter."
  (multiple-value-bind
    (secs hour minute day month year)
    (decode-universal-time current-date)
    (declare (ignore secs hour minute year))
    (when (or (and (equal 1 day) (equal 1 month))
              (and (equal 1 day) (equal 4 month))
              (and (equal 1 day) (equal 7 month))
              (and (equal 1 day) (equal 10 month)))
      (incf (stock-position-cash stock-position) 6000)))
  (follow-advice stock-position current-date stock-price strategy-fn))

(defun yearly-investment (stock-position current-date stock-price strategy-fn)
  "Start with 10K, and invest 24K each year."
  (multiple-value-bind
    (secs hour minute day month year)
    (decode-universal-time current-date)
    (declare (ignore secs hour minute year))
    (when (and (equal 1 day) (equal 1 month))
      (incf (stock-position-cash stock-position) 24000)))
  (follow-advice stock-position current-date stock-price strategy-fn))

(defun get-aim-scenario ()
  (let ((stock-position-control 0) (time-to-trade t))
    (defun aim-scenario (stock-position current-date stock-price strategy-fn)
      (declare (ignore strategy-fn))
      (if (zerop stock-position-control)
        (setf stock-position-control 5000))
      (multiple-value-bind
        (secs hour minute day month year)
        (decode-universal-time current-date)
        (declare (ignore secs hour minute month year))
        (if (equal 15 day)
          (setf time-to-trade t)))
      (if (and time-to-trade (equal current-date (stock-price-date stock-price)))
        (let* ((stock-value (* (stock-position-quantity stock-position) (stock-price-close stock-price)))
              (safe (* 1/10 stock-value))
              (advice (- stock-position-control stock-value))
              (order-value (- (abs advice) safe)))
          (setf time-to-trade nil)
          (if (> order-value (* 50 (stock-price-close stock-price)))
            (progn
              (if (and (> advice 0) (>= (stock-position-cash stock-position) order-value))
                (let ((cash-before-purchase (stock-position-cash stock-position)))
                  (perform-buy stock-position stock-price order-value)
                  (incf stock-position-control (/ (- cash-before-purchase (stock-position-cash stock-position)) 2))))
              (if (< advice 0)
                (perform-sell stock-position stock-price t (* (truncate (* (/ order-value stock-value) (stock-position-quantity stock-position)) 50) 50))))))))))
