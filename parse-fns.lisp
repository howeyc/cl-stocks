;;;; parse-fns.lisp

(in-package #:cl-stocks)

(defun parse-year (line)
  (parse-integer line :start 0 :end 4))
(defun parse-month (line)
  (parse-integer line :start 5 :end 7))
(defun parse-day (line)
  (parse-integer line :start 8 :end 10))
(defun parse-date (line)
  (encode-universal-time 0 0 0 (parse-day line) (parse-month line) (parse-year line)))
