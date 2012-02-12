;;;; strategies.lisp

(in-package #:cl-stocks)

(defun in-the-bank (stock-price)
 (declare (ignore stock-price))
 (values "Hold" 0))

(defun buy-when-possible (stock-price)
 (declare (ignore stock-price))
 (values "Buy" 1))

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
  (values "Buy" (+ (/ 1 2) (- 1 (/ (- (stock-price-close stock-price) (stock-price-52wl stock-price)) (- (stock-price-52wh stock-price) (stock-price-52wl stock-price)))))))

;;; DRIP Investment Calculator (http://www.futuresforextrading.com/dripadvisor/calculator.htm)
(defun drip-invest-calc (stock-price)
  (let* ((expected-growth-factor (/ 215 200))    ; Multiplication factor (eg 1.075) that is half of 15%
         (expected-price (* expected-growth-factor (stock-price-sma stock-price)))
         (valuation-deviation (/ (- (stock-price-close stock-price) expected-price) expected-price)))
    (values "Buy" (- 1 (* 3 valuation-deviation)))))          ; According to the site the factor of 3 depends on the choice of 15% some how??

;;; Twinvest (http://www.aim-users.com/twinvest.htm)
(defun get-twinvest-fn ()
  (let ((code 0))
    (defun twinvest (stock-price)
      (if (zerop code)
        (setf code (* 2000 (/ 3 4) (stock-price-close stock-price))))
      (values "Buy" (/ code (stock-price-close stock-price) 2000)))))
