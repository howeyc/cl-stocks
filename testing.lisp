(defun reload ()
  (asdf:load-system "cl-stocks"))
(reload)

(defun run-test-ge ()
  (format t "~%GE:~%")
  (cl-stocks:run-them-all "test-data/ge-prices.csv" "test-data/ge-divs.csv" "test-data/ge-splits.csv" t))

(defun run-test-k ()
  (format t "~%K:~%")
  (cl-stocks:run-them-all "test-data/k-prices.csv" "test-data/k-divs.csv" "test-data/k-splits.csv" t))

(defun run-tests ()
  (run-test-ge)
  (run-test-k))

(defun reload-tests ()
 (load "testing.lisp"))
