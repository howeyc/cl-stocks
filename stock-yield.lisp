;;; stock-yield.lisp
;;;
;;; Example Usage:
;;; (get-year-yields "GE" '(1 1 2000) '(1 1 2012))
;;;

(in-package #:cl-stocks)

(defun get-year-max-min (ticker start end)
  (loop for (year . row-vals) in (group-by:group-by (rest (cl-yahoo-finance:read-historical-data ticker start end :historical-type :monthly)) :key (cl-utilities:compose #'parse-year #'first) :value #'identity)
        collect (list year (reduce #'max row-vals :key #'third) (reduce #'min row-vals :key #'fourth))))

(defun get-year-divs (ticker start end)
  (loop for (year . row-vals) in (group-by:group-by (rest (cl-yahoo-finance:read-historical-data ticker start end :historical-type :dividends_only)) :key (cl-utilities:compose #'parse-year #'first) :value #'identity)
	      collect (list year (reduce #'+ row-vals :key #'second))))

(defun get-year-yields (ticker start end)
  (let ((year-price-divs (group-by:group-by (append (get-year-max-min ticker start end) (get-year-divs ticker start end)))))
    (cons
     '("Year" "52w High" "52w Low" "Annual Div" "Low Yield" "High Yield")
	   (loop for (year (high low) (div)) in year-price-divs
           collect (list year high low div (* 100 (/ div high)) (* 100 (/ div low)))))))
