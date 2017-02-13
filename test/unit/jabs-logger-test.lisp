;; (defvar *current-output-registry* nil)
;; (defvar *log-string* "#Y-#m-#d #H:#M:#S [#L]: #R"
;; (defvar *month-names* '("Jan" "Feb" "Mar"
;; (defvar *day-of-week-names* '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
;; (defvar *log-disable-notes* nil)
;; (defvar *log-level* "ERROR")
;; (defvar *log-quiet-p* nil)
;; (defvar *log-levels* '("CRITICAL" "ERROR" "WARNING" "INFO" "DEBUG"))
;; (defvar *log-trace-p* nil)
;; (defvar *fail-on-critical* t)
;; (defvar *fail-on-error* nil)
;; (defun terminate (status)
;; (defun date-get-timezone (date)
;; (defun date-get-year (date &key (short nil) timezone)
;; (defun date-get-year-short (date &key timezone)
;; (defun date-get-month (date &key timezone human-readable-p)
;; (defun date-get-month-hr (date &key timezone)
;; (defun date-get-day (date &key timezone)
;; (defun date-get-hour (date &key timezone)
;; (defun date-get-minute (date &key timezone)
;; (defun date-get-second (date &key timezone)
;; (defun date-get-doy (date &key timezone)
;; (defun date-get-dow (date &key timezone human-readable-p)
;; (defun date-get-dow-hr (date &key timezone)
;; (defvar *date-symbols* (make-hash-table :test 'equal))
;; (defun make-log-string (date level title message &optional (format *log-string*))
;; (defvar *log-output-types* (make-hash-table))
;; (defvar *log-used-output-types* nil)
;; (defvar *log-file* "/tmp/jabs.log")
;; (defun check-output-type-args (args)
;; (defun level> (level1 level2)
;; (defun level< (level1 level2)
;; (defun log-write-output (level title string)
;; (defun get-stamp (&optional (stamp "STAMP"))
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
