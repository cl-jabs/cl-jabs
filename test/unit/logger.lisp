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
(load (make-pathname :directory '(:relative "test" "unit") :name "header.lisp" :type "in"))

(defpackage logger-test
  (:use :cl :test-engine :clunit :logger@jabs))

(in-package :logger-test)

(defvar *mock-msg* "")

(defun check-msg (msg)
  (when (string-contain-p *mock-msg* msg) t))

(setf jlog:*log-string* "#Y-#m-#d #H:#M:#S #L: #R")

(define-output-type :mock-type (date type title msg)
  (setf *mock-msg* (make-log-string date type title msg *log-string*)))

(enable-output-type :mock-type)
(disable-output-type :terminal)

;; tests
(defsuite logger-suite ()
  )

(deftest check-log-string-external-symbol (logger-suite)
  (assert-true (eq (nth-value 1 (find-symbol "*LOG-STRING*" :logger@jabs))
                   :external)))

(deftest logger-test-messages (logger-suite)
  (assert-true (progn (err "test")
                      (check-msg "ERROR: test")))
  (assert-false (progn (wrn "test")
                       (check-msg "WARNING: test")))
  (assert-true (progn
                 (setf *log-level* "WARNING")
                 (wrn "test")
                 (check-msg "WARNING: test")))
  (assert-false (progn
                 (setf *log-disable-notes* t)
                 (note "test")
                 (check-msg "NOTE: test"))))

(format t "~a~%" (run-suite 'logger-suite :stop-on-fail nil :report-progress nil))
