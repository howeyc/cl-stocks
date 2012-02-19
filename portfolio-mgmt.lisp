;;;; portfolio-mgmt.lisp

(in-package #:cl-stocks)

(defun get-ticker-hash (tickers start end)
  (let ((ticker-hash (make-hash-table :test 'equal)))
    (mapcar #'(lambda (ticker)
                (multiple-value-bind 
                  (stock-prices dividends splits)
                  (get-stock-info ticker start end)
                  (let ((stock-price-hash (make-hash-table :test 'equal))
                        (stock-div-hash (make-hash-table :test 'equal))
                        (stock-split-hash (make-hash-table :test 'equal)))
                    (dolist (stock-price stock-prices)
                      (setf (gethash (iso-date (stock-price-date stock-price)) stock-price-hash) stock-price))
                    (dolist (stock-div dividends)
                      (setf (gethash (iso-date (stock-div-date stock-div)) stock-div-hash) stock-div))
                    (dolist (stock-split splits)
                      (setf (gethash (iso-date (stock-split-date stock-split)) stock-split-hash) stock-split))
                    (setf (gethash ticker ticker-hash) (list (make-instance 'stock-position :cash 0) stock-price-hash stock-div-hash stock-split-hash)))))
            tickers)
    ticker-hash))

(defun pick-max (position-price-list strategy-fn cash-to-inject)
  (let ((current-max 0) (position-to-modify nil) (unused-cash 0) (stock-price nil))
    (dolist (position-price position-price-list)
      (multiple-value-bind
        (advice-string advice-factor force)
        (funcall strategy-fn (cdr position-price))
        (when (string-equal "Sell" advice-string)
          (follow-advice (car position-price) (stock-price-date (cdr position-price)) (cdr position-price) strategy-fn)
          (incf cash-to-inject (stock-position-cash (car position-price)))
          (setf (stock-position-cash (car position-price)) 0))
        (when (and (string-equal "Buy" advice-string) (> advice-factor current-max))
          (setf current-max advice-factor)
          (setf stock-price (cdr position-price))
          (setf position-to-modify (car position-price)))))
    (when (not (null position-to-modify))
      (incf (stock-position-cash position-to-modify) cash-to-inject)
      (setf cash-to-inject 0)
      (follow-advice position-to-modify (stock-price-date stock-price) stock-price strategy-fn)
      (setf unused-cash (stock-position-cash position-to-modify))
      (decf (stock-position-cash position-to-modify) unused-cash))
    (+ cash-to-inject unused-cash)))

(defun pick-avg (position-price-list strategy-fn cash-to-inject)
  (dolist (position-price position-price-list 0)
    (incf (stock-position-cash (car position-price)) (/ cash-to-inject (length position-price-list)))
    (follow-advice (car position-price) (stock-price-date (cdr position-price)) (cdr position-price) strategy-fn)))

(defun pick-wavg (position-price-list strategy-fn cash-to-inject)
  (let ((total 0))
    (dolist (position-price position-price-list)
      (multiple-value-bind
        (advice-string advice-factor force)
        (funcall strategy-fn (cdr position-price))
        (when (string-equal "Buy" advice-string)
          (incf total advice-factor))))
    (dolist (position-price position-price-list 0)
      (multiple-value-bind
        (advice-string advice-factor force)
        (funcall strategy-fn (cdr position-price))
        (when (string-equal "Buy" advice-string)
          (incf (stock-position-cash (car position-price)) (* cash-to-inject (/ advice-factor total)))))
      (follow-advice (car position-price) (stock-price-date (cdr position-price)) (cdr position-price) strategy-fn))))

(defun get-ticker-transactions (ticker current-stock-position)
 (loop for trans in (stock-position-transactions current-stock-position)
  collect (cons ticker trans)))

(defun sort-ticker-transactions (ticker-trans-list)
  (sort ticker-trans-list #'(lambda (x y)
                              (if (and (= (transaction-date (cdr x)) (transaction-date (cdr y))) (string-equal "Sell" (transaction-type (cdr x))))
                                t
                                (< (transaction-date (cdr x)) (transaction-date (cdr y)))))))

(defun run-a-portfolio (ticker-hash tickers start end interest-fn strategy-fn pick-fn)
  (let ((cash 10000) (last-price-hash (make-hash-table :test 'equal)) (transactions nil))
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
              (let ((current-stock-position (first (gethash ticker ticker-hash)))
                    (current-stock-price (gethash (iso-date date) (second (gethash ticker ticker-hash))))
                    (current-dividend (gethash (iso-date date) (third (gethash ticker ticker-hash))))
                    (current-split (gethash (iso-date date) (fourth (gethash ticker ticker-hash)))))
                (when current-stock-price
                  (setf (gethash ticker last-price-hash) current-stock-price)
                  (push (cons current-stock-position current-stock-price) position-price-list))
                (when current-dividend
                  (collect-div current-stock-position current-dividend))
                (when current-split
                  (collect-split current-stock-position current-split))))
            (if (not (null position-price-list))
              (setf cash (funcall pick-fn position-price-list strategy-fn cash)))))
    (dolist (ticker tickers)
      (let ((current-stock-position (first (gethash ticker ticker-hash))))
        (perform-sell current-stock-position (gethash ticker last-price-hash) t (stock-position-quantity current-stock-position))
        (incf cash (stock-position-cash current-stock-position))
        (setf (stock-position-cash current-stock-position) 0)
        (setf transactions (append transactions (get-ticker-transactions ticker current-stock-position)))))
    (values 
     cash
     (sort-ticker-transactions transactions))))
