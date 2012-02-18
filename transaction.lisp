;;;; transaction.lisp

(in-package #:cl-stocks)

(defclass transaction ()
  ((date :accessor transaction-date
         :initarg :date
         :initform 0)
   (type :accessor transaction-type
         :initarg :type
         :initform "")
   (cash :accessor transaction-cash
         :initarg :cash
         :initform 0)
   (quantity :accessor transaction-quantity
             :initarg :quantity
             :initform 0)
   (price :accessor transaction-price
          :initarg :price
          :initform 0)
   (accum-cash :accessor transaction-accum-cash
               :initarg :accum-cash
               :initform 0)
   (accum-quantity :accessor transaction-accum-quantity
                   :initarg :accum-quantity
                   :initform 0)))

(defmethod print-object ((tran transaction) stream)
  (format stream "~A ~7A ~10,2F ~7D ~6,2F ~10,2F ~7D"
          (human-date (transaction-date tran))
          (transaction-type tran)
          (transaction-cash tran)
          (transaction-quantity tran)
          (transaction-price tran)
          (transaction-accum-cash tran)
          (transaction-accum-quantity tran)))
