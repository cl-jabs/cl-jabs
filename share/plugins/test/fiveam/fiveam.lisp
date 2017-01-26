(in-package :fiveam@test@plugin@jabs)

;; (use-package :fiveam)

(defvar *test-dribble* t) ;; should be an output stream. TODO: process it!

(setf fiveam:*on-error* :debug)
(setf fiveam:*debug-on-error* t)
(setf fiveam:*debug-on-failure* t)

(let (suite suite-file)
  
  (defun get-def-suite-file-from-list (name list)
    (cond ((null list) nil)
	  ((and
	    (string-equal (tostr (car list)) "DEF-SUITE")
	    (string-equal (tostr (cadr list)) (tostr name)))
	   (setf suite (tostr (cadr list))))
	  ((atom (car list))
	   (get-def-suite-file-from-list name (cdr list)))
	  (t
	   (get-def-suite-file-from-list name (car list))
	   (get-def-suite-file-from-list name (cdr list)))))

  (defun clear-def-suite ()
    (setf suite nil))

  (defun get-def-suite-file (name suite-files)
    (clear-def-suite)
    (let ((files suite-files))
      (dolist (f files)
	(get-def-suite-file-from-list name (os-cat f :list))
	(when suite (setf suite-file f))))
    suite-file)
  )

(defun run-single-test ()
  (format t "~%``run-single-test`` IS NOT IMPLEMENTED~%~%"))

(defun run-single-suite (name)
  (let ((suite-file (get-def-suite-file name (get-project-test-files *jabs-current-project* (find-plugin :fiveam :test)))))
    (jlog:info "Running suite: ``~a'' in file ``~a''" name suite-file)
    (load suite-file)
    (fiveam:run! name)))

(defun run-all-tests ()
  (let ((testfiles (get-project-test-files *jabs-current-project* (find-plugin :fiveam :test))))
    (jlog:info "Running all tests from files: ``~a''" testfiles)
    (dolist (file testfiles)
      (load file))
    (fiveam:run!)))

;; (format t "~%``run-all-tests`` IS NOT IMPLEMENTED~%~%"))
