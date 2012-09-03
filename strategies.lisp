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

(defun sma-rule (stock-price)
 (let ((price (stock-price-close stock-price)) (sma (stock-price-sma stock-price)))
  (if (> (abs (- sma price)) (/ price 10))
   (if (> price sma)
    (values "Sell" 1)
    (values "Buy" 1))
   (values "Hold" 0))))

(defun get-crazy-market-fn (percent)
 "When there is a pull back from the high of more than %, BUY
 If we have a position, and we can make %, SELL"
 (let ((stock-pos :OUT) (last-max 0) (last-buy 0))
  (defun crazy-market (stock-price)
   (when (> (stock-price-close stock-price) last-max)
    (setf last-max (stock-price-close stock-price)))
   (cond ((and (eq stock-pos :OUT) (< (stock-price-close stock-price) (* (/ (- 100 percent) 100) last-max)))
          (setf last-buy (stock-price-close stock-price) stock-pos :IN)
          (values "Buy" 1))
         ((and (eq stock-pos :IN) (> (stock-price-close stock-price) (* (/ (+ 100 percent) 100) last-buy)))
          (setf last-max (stock-price-close stock-price) stock-pos :OUT)
          (values "Sell" 1))
         (t (values "Hold" 0))))))

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

;;; Dividends Don't Lie
(defun get-div-dont-lie (ticker start end)
 (let ((stock-yields (rest (get-year-yields ticker start end))))
  (defun dividends-dont-lie (stock-price)
   (multiple-value-bind
    (secs hour minute day month year)
    (decode-universal-time (stock-price-date stock-price))
    (declare (ignore secs hour minute day month))
    (let* ((prev-year-low-yield (or (fifth (find (1- year) stock-yields :key #'first))
                                    (fifth (first (last stock-yields)))))
           (prev-year-high-yield (or (sixth (find (1- year) stock-yields :key #'first))
                                     (sixth (first (last stock-yields)))))
           (prev-year-div (or (fourth (find (1- year) stock-yields :key #'first))
                              (fourth (first (last stock-yields)))))
           (current-yield (* 100 (/ prev-year-div (stock-price-close stock-price)))))
     (cond ((< current-yield (* 1.1 prev-year-low-yield))
            (values "Buy" 1))
           ((> current-yield (* 0.9 prev-year-high-yield))
            (values "Sell" 1))
           (t (values "Hold" 0))))))))
