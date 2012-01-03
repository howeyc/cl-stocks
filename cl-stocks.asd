;;;; cl-stocks.asd

(asdf:defsystem #:cl-stocks
  :serial t
  :depends-on (#:cl-csv
               #:parse-number)
  :components ((:file "package")
               (:file "date")
               (:file "sma")
               (:file "transaction")
               (:file "portfolio")
               (:file "stock-price")
               (:file "parse-csv")
               (:file "strategies")
               (:file "scenarios")
               (:file "cl-stocks")))

