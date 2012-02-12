;;;; stock-price.lisp

(in-package #:cl-stocks)

(defclass stock-price ()
  ((date :accessor stock-price-date
         :initarg :date
         :initform 0)
   (open :accessor stock-price-open
         :initarg :open
         :initform 0)
   (close :accessor stock-price-close
          :initarg :close
          :initform 0)
   (sma :accessor stock-price-sma
        :initarg :sma
        :initform 0)
   (year-high :accessor stock-price-52wh
              :initform 0)
   (year-low :accessor stock-price-52wl
             :initform 0)))

(defmethod print-object ((stock-price stock-price) stream)
 (format stream "Date: ~A, Close: ~A, 52wHigh: ~A, 52wLow: ~A"
  (human-date (stock-price-date stock-price))
  (stock-price-close stock-price)
  (stock-price-52wh stock-price)
  (stock-price-52wl stock-price)))

(defclass stock-div ()
  ((date :accessor stock-div-date
         :initarg :date
         :initform 0)
   (div :accessor stock-div-div
        :initarg :open
        :initform 0)))

(defclass stock-split ()
  ((date :accessor stock-split-date
         :initarg :date
         :initform 0)
   (div :accessor stock-split-factor
        :initarg :open
        :initform 0)))

(defun add-52w-numbers (stock-prices)
 (let ((queue (list (make-instance 'stock-price))))
  (dolist (stock-price stock-prices stock-prices)
   (if (>= (length queue) 52)
    (pop queue))
   (setf queue (nconc queue (list stock-price)))
   (setf (stock-price-52wh stock-price) (apply #'max (mapcar #'(lambda (x) (stock-price-close x)) queue)))
   (setf (stock-price-52wl stock-price) (apply #'min (mapcar #'(lambda (x) (stock-price-close x)) queue))))))
