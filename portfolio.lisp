;;;; portfolio.lisp

(in-package #:cl-stocks)

(defclass portfolio ()
  ((cash :accessor portfolio-cash
         :initarg :cash
         :initform 1000000000)
   (quantity :accessor portfolio-quantity
             :initarg :quantity
             :initform 0)
   (avg-price :accessor portfolio-avg-price
              :initarg :avg-price
              :initform 0)
   (transactions :accessor portfolio-transactions
                 :initarg :transactions
                 :initform nil)))

(defun get-daily-interest-rate-accumulator (annual-rate)
  (let ((daily-rate (- (expt (+ 1 annual-rate) (/ 1 365)) 1)))
    (lambda (portfolio) (incf (portfolio-cash portfolio) (floor (* (portfolio-cash portfolio) daily-rate))))))

(defmethod print-object ((port portfolio) stream)
 (format stream "Cash: ~A, Quantity: ~A ~%" (portfolio-cash port) (portfolio-quantity port)))

