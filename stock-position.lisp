;;;; stock-position.lisp

(in-package #:cl-stocks)

(defclass stock-position ()
  ((cash :accessor stock-position-cash
         :initarg :cash
         :initform 10000)
   (quantity :accessor stock-position-quantity
             :initarg :quantity
             :initform 0)
   (avg-price :accessor stock-position-avg-price
              :initarg :avg-price
              :initform 0)
   (transactions :accessor stock-position-transactions
                 :initarg :transactions
                 :initform nil)))

(defun get-daily-interest-rate (annual-rate)
  (let ((daily-rate (- (expt (+ 1 annual-rate) (/ 1 365)) 1)))
    (lambda (cash) (* cash daily-rate))))

(defmethod print-object ((spos stock-position) stream)
 (format stream "Cash: ~A, Quantity: ~A ~%" (stock-position-cash spos) (stock-position-quantity spos)))

