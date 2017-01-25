(in-package :fiveam@test@plugin@jabs)

;; (use-package :fiveam)

(defun run-single-test ()
  (format t "~%``run-single-test`` IS NOT IMPLEMENTED~%~%"))

(defun run-single-suite ()
  (format t "~%``run-single-suite`` IS NOT IMPLEMENTED~%~%"))

(defun run-all-tests ()
  (jlog:err "~a" (get-project-test-files *jabs-current-project* (find-plugin :fiveam :test))))
;; (format t "~%``run-all-tests`` IS NOT IMPLEMENTED~%~%"))
