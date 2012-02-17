;;;; date.lisp

(in-package #:cl-stocks)

(defun human-date (date)
  (multiple-value-bind
    (secs hour minute day month year)
    (decode-universal-time date)
    (declare (ignore secs hour minute))
    (format nil "~4D/~2,'0D/~2,'0D" year month day)))

(defun iso-date (date)
 (multiple-value-bind
  (secs hour minute day month year)
  (decode-universal-time date)
  (declare (ignore secs hour minute))
  (format nil "~4D-~2,'0D-~2,'0D" year month day)))
