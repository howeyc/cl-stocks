(defun reload ()
  (ql:quickload "cl-stocks"))
(reload)

(defun run-tests ()
  (format t "~%GE:~%")
  (cl-stocks:run-all-scenarios "GE" '(1 1 1990) '(1 1 2012) t)
  (format t "~%K:~%")
  (cl-stocks:run-all-scenarios "K" '(1 1 1990) '(1 1 2012) t))

(defun portfolio-tests ()
  (format t "~%Portfolio Test:~%")
  (cl-stocks:run-portfolios '(K GE) '(1 1 1990) '(1 1 2012) file))

(defun reload-tests ()
 (load "testing.lisp"))
