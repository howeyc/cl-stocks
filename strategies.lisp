;;;; strategies.lisp

(in-package #:cl-stocks)

(defun in-the-bank (stock-price)
 (declare (ignore stock-price))
 (values "Hold" 0))

(defun buy-when-possible (stock-price)
 (declare (ignore stock-price))
 (values "Buy" 1))

(defun random-advice (stock-price)
  (declare (ignore stock-price))
  (values
    (case (random 3)
      (0 "Buy")
      (1 "Sell")
      (2 "Hold"))
    1))

(defun thirty-percent-rule (stock-price)
  (let* ((diff (- (stock-price-52wh stock-price) (stock-price-52wl stock-price)))
         (thirty-of-diff (* (/ 30 100) diff))
         (bottom-thirty (+ (stock-price-52wl stock-price) thirty-of-diff))
         (top-thirty (- (stock-price-52wh stock-price) thirty-of-diff)))
    (cond ((<= (stock-price-close stock-price) bottom-thirty) (values "Buy" 1))
          ((>= (stock-price-close stock-price) top-thirty) (values "Sell" 1))
          (t (values "Hold" 0)))))

;;; Moneypaper Invest%
;;; INVEST%: ((1 - ((Current Price - 52WeekLow) / ( 52WeekHigh - 52WeekLow))) + 0.5)
(defun moneypaper-invest (stock-price)
  (+ (/ 1 2) (- 1 (/ (- (stock-price-close stock-price) (stock-price-52wl stock-price)) (- (stock-price-52wh stock-price) (stock-price-52wl stock-price))))))

;;; DRIP Investment Calculator (http://www.futuresforextrading.com/dripadvisor/calculator.htm)
(defun drip-invest-calc (stock-price)
  (let* ((expected-growth-factor (/ 215 200))    ; Multiplication factor (eg 1.075) that is half of 15%
         (expected-price (* expected-growth-factor (stock-price-sma stock-price)))
         (valuation-deviation (/ (- (stock-price-close stock-price) expected-price) expected-price)))
    (- 1 (* 3 valuation-deviation))))           ; According to the site the factor of 3 depends on the choice of 15% some how??

;;; Twinvest (http://www.aim-users.com/twinvest.htm)
(defun get-twinvest-fn ()
  (let ((code 0))
    (defun twinvest (stock-price)
      (if (zerop code)
        (setf code (* 200000000 (/ 3 4) (stock-price-close stock-price))))
      (/ code (stock-price-close stock-price) 200000000))))

;;; AIM
(defun get-aim-fn()
  (let ((quantity 0) (stock-value 0) (cash 0) (safe 0) (order-value 0) (portfolio-control 0) (advice 0) (buy-factor 0))
    (defun automatic-investment (stock-price)
      (if (zerop portfolio-control)
        (progn
          (setf portfolio-control 500000000 
                 stock-value 500000000 
                 quantity (/ stock-value (stock-price-close stock-price))
                 cash 500000000)
          (values "Buy" (/ 1 2)))
        (progn
          (setf stock-value (* quantity (stock-price-close stock-price))
                 advice (- portfolio-control stock-value)
                 safe (* (/ 1 10) stock-value)
                 order-value (- (abs advice) safe))
          (if (> order-value (* 50 (stock-price-close stock-price)))
            (if (> advice 0)
              (progn
                (when (< cash order-value) (setf order-value cash)) 
                (unless (zerop cash) (setf buy-factor (/ order-value cash)))
                (incf portfolio-control (/ order-value 2))
                (decf cash order-value)
                (incf quantity (/ order-value (stock-price-close stock-price)))
                (values "Buy" buy-factor))
              (progn
                (decf quantity (/ order-value (stock-price-close stock-price)))
                (incf cash order-value)
                (values "Sell" (/ order-value stock-value))))
            (values "Hold" 0)))))))

 
