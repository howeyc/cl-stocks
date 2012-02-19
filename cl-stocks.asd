;;;; cl-stocks.asd

(asdf:defsystem #:cl-stocks
  :serial t
  :depends-on (#:cl-csv
               #:cl-yahoo-finance)
  :components ((:file "package")
               (:file "date")
               (:file "sma")
               (:file "transaction")
               (:file "stock-position")
               (:file "stock-price")
               (:file "parse-fns")
               (:file "get-stock-info")
               (:file "strategies")
               (:file "scenarios")
               (:file "portfolio-mgmt")
               (:file "cl-stocks"))
  :name "cl-stocks"
  :version "0.1.0"
  :maintainer "Chris Howey"
  :author "Chris Howey"
  :license "ISC"
  :description "Test stock purchasing strategies")

