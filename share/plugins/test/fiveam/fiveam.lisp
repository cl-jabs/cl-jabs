;;; -*- Mode: Lisp -*-
#|
MIT License

Copyright (c) 2017 Alexander Vynnyk <cosmonaut.ok@zoho.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|#
(in-package :fiveam@test@plugin@jabs)

;; (use-package :fiveam)

(defvar *test-dribble* t) ;; should be an output stream. TODO: process it!

(when *jabs-test-fail-on-error*
  ;; (setf fiveam:*on-error* :debug)
  (setf fiveam:*debug-on-error* t)
  (setf fiveam:*debug-on-failure* t))

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

(defun run-single-test (&rest rest)
  (declare (ignore rest))
  (format t "~%``run-single-test`` IS NOT IMPLEMENTED~%~%"))

(defun run-single-suite (name)
  (let ((suite-file (get-def-suite-file name
                                        (get-project-test-files *jabs-current-project*
                                                                (find-plugin :fiveam :test)))))
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
