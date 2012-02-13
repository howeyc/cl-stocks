;;;; portfolio-mgmt.lisp

(in-package #:cl-stocks)

(defun get-ticker-hash (tickers start end)
  (let ((ticker-hash (make-hash-table :test 'equal)))
    (mapcar #'(lambda (ticker)
                (multiple-value-bind 
                  (stock-prices dividends splits)
                  (get-stock-info ticker start end)
                  (setf (gethash ticker ticker-hash) (list (make-instance 'stock-position :cash 0) stock-prices dividends splits))))
            tickers)
    ticker-hash))

(defun get-ticker-field (record column del)
  (let ((val (first (nth column record))))
    (if del
      (setf (nth column record) (rest (nth column record))))
    val))

(defun pick-max (position-price-list strategy-fn cash-to-inject)
  (let ((current-max 0) (position-to-modify nil) (unused-cash 0) (stock-price nil))
    (dolist (position-price position-price-list)
      (multiple-value-bind
        (advice-string advice-factor force)
        (funcall strategy-fn (cdr position-price))
        (if (> advice-factor current-max)
          (progn 
            (setf current-max advice-factor)
            (setf stock-price (cdr position-price))
            (setf position-to-modify (car position-price))))))
    (incf (stock-position-cash position-to-modify) cash-to-inject)
    (follow-advice position-to-modify (stock-price-date stock-price) stock-price strategy-fn)
    (setf unused-cash (stock-position-cash position-to-modify))
    (decf (stock-position-cash position-to-modify) unused-cash)
    unused-cash))

(defun run-a-portfolio (tickers start end interest-fn strategy-fn pick-fn)
  (let ((ticker-hash (get-ticker-hash tickers start end)) (cash 10000) (last-price-hash (make-hash-table :test 'equal)))
    (loop for date from (encode-universal-time 0 0 0 (second start) (first start) (third start) 0) to (encode-universal-time 0 0 0 (second end) (first end) (third end) 0) by 86400
          do
          (incf cash (funcall interest-fn cash))
          (multiple-value-bind
            (secs hour minute day month year)
            (decode-universal-time date)
            (declare (ignore secs hour minute month year))
            (when (equal 1 day)
              (incf cash 2000)))
          (let ((position-price-list nil))
            (dolist (ticker tickers)
              (let ((ticker-info (gethash ticker ticker-hash)))
                (when (and (get-ticker-field ticker-info 1 nil) (>= date (stock-price-date (get-ticker-field ticker-info 1 nil))))
                  (setf (gethash ticker last-price-hash) (get-ticker-field ticker-info 1 nil))
                  (push (cons (first ticker-info) (get-ticker-field ticker-info 1 t)) position-price-list))
                (if (and (get-ticker-field ticker-info 2 nil) (>= date (stock-div-date (get-ticker-field ticker-info 2 nil))))
                  (collect-div (first ticker-info) (get-ticker-field ticker-info 2 t)))
                (if (and (get-ticker-field ticker-info 3 nil) (>= date (stock-split-date (get-ticker-field ticker-info 3 nil))))
                  (collect-split (first ticker-info) (get-ticker-field ticker-info 3 t)))))
            (if (not (null position-price-list))
              (setf cash (funcall pick-fn position-price-list strategy-fn cash)))))
  (dolist (ticker tickers)
    (perform-sell (first (gethash ticker ticker-hash)) (gethash ticker last-price-hash) t (stock-position-quantity (first (gethash ticker ticker-hash))))
    (incf cash (stock-position-cash (first (gethash ticker ticker-hash)))))
  cash))