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
  (string-contain-p *mock-msg* msg))

(setf jlog:*log-string* "#Y-#m-#d #H:#M:#S #L: #R")

(define-output-type :mock-type (date type title msg)
  (setf *mock-msg* (make-log-string date type title msg *log-string*)))

(enable-output-type :mock-type)
(disable-output-type :terminal)

;; tests
(defsuite logger-suite ()
  )

(deffixture logger-suite (@body)
  (let ((now 3696018717) ; just don'tset dynamical time ;)
        (*log-level* "ERROR"))
    @body))

(deftest check-log-string-external-symbol (logger-suite)
  (assert-equal :external (nth-value 1 (find-symbol "*LOG-STRING*" :logger@jabs))))

(deftest logger-test-messages (logger-suite)
  (assert-true (progn (err "test") (check-msg "ERROR: test")))
  (assert-false (progn (wrn "test") (check-msg "WARNING: test")))
  (assert-true (progn (setf *log-level* "WARNING") (wrn "test") (check-msg "WARNING: test")))
  (assert-false (progn (setf *log-disable-notes* t) (note "test") (check-msg "NOTE: test"))))

;; (defun terminate (status)
(deftest date-get-timezone-test (logger-suite)
    (assert-equal 2 (jlog::date-get-timezone now)))

(deftest date-get-year-test (logger-suite)
  (assert-equal "2017" (jlog::date-get-year now))
  (assert-equal "2017" (jlog::date-get-year now :timezone 11))
  (assert-equal "1899" (jlog::date-get-year 1))
  (assert-condition type-error (jlog::date-get-year :zzz)))

(deftest date-get-year-short-test (logger-suite)
  (assert-equal "17" (jlog::date-get-year-short now))
  (assert-equal "17" (jlog::date-get-year-short now :timezone 11))
  (assert-equal "99" (jlog::date-get-year-short 1))
  (assert-condition type-error (jlog::date-get-year-short :zzz)))

(deftest date-get-month-test (logger-suite)
  (assert-equal "02" (jlog::date-get-month now))
  (assert-equal "02" (jlog::date-get-month now :timezone 11))
  (assert-equal "Feb" (jlog::date-get-month now :timezone 11 :human-readable-p t))
  (assert-equal "01" (jlog::date-get-month 1))
  (assert-condition type-error (jlog::date-get-month :zzz)))

(deftest date-get-month-hr-test (logger-suite)
  (assert-equal "Feb" (jlog::date-get-month-hr now))
  (assert-equal "Feb" (jlog::date-get-month-hr now :timezone 11))
  (assert-equal "Jan" (jlog::date-get-month-hr 1))
  (assert-condition type-error (jlog::date-get-month-hr :zzz)))


(deftest date-get-day-test (logger-suite)
  (assert-equal "14" (jlog::date-get-day now))
  (assert-equal "14" (jlog::date-get-day now :timezone 11))
  (assert-equal "01" (jlog::date-get-day 1))
  (assert-condition type-error (jlog::date-get-day :zzz)))


(deftest date-get-hour-test (logger-suite)
  (assert-equal "01" (jlog::date-get-hour now))
  (assert-equal "10" (jlog::date-get-hour now :timezone 11))
  (assert-equal "02" (jlog::date-get-hour 3))
  (assert-condition type-error (jlog::date-get-hour :zzz)))

(deftest date-get-minute-test (logger-suite)
  (assert-equal "51" (jlog::date-get-minute now))
  (assert-equal "51" (jlog::date-get-minute now :timezone 11))
  (assert-equal "02" (jlog::date-get-minute 1))
  (assert-condition type-error (jlog::date-get-minute :zzz)))

(deftest date-get-second-test (logger-suite)
  (assert-equal "57" (jlog::date-get-second now))
  (assert-equal "57" (jlog::date-get-second now :timezone 11))
  (assert-equal "05" (jlog::date-get-second 1))
  (assert-condition type-error (jlog::date-get-second :zzz)))


(deftest date-get-doy-test (logger-suite) ;; TODO: fix it to strings
  (assert-equal "45" (jlog::date-get-doy now))
  (assert-equal "44" (jlog::date-get-doy now :timezone 11))
  (assert-equal "1" (jlog::date-get-doy 1))
  (assert-condition type-error (jlog::date-get-doy :zzz)))


(deftest date-get-dow-test (logger-suite)
  (assert-equal "2" (jlog::date-get-dow now))
  (assert-equal "2" (jlog::date-get-dow now :timezone 11))
  (assert-equal "Tue" (jlog::date-get-dow now :timezone 11 :human-readable-p t))
  (assert-equal "1" (jlog::date-get-dow 1))
  (assert-condition type-error (jlog::date-get-dow :zzz)))


(deftest date-get-dow-hr-test (logger-suite)
  (assert-equal "Tue" (jlog::date-get-dow-hr now))
  (assert-equal "Tue" (jlog::date-get-dow-hr now :timezone 11))
  (assert-equal "Mon" (jlog::date-get-dow-hr 1))
  (assert-condition type-error (jlog::date-get-dow-hr :zzz)))

(deftest date-symbols-test (logger-suite)
  (assert-equal 'hash-table (type-of jlog::*date-symbols*)))

;; (defun make-log-string (date level title message &optional (format *log-string*))

(deftest log-output-types-test (logger-suite)
  (assert-equal 'hash-table (type-of jlog::*log-output-types*)))

;; (defun check-output-type-args (args)
(deftest level>-test (logger-suite)
  (assert-true (jlog::level> "DEBUG" "INFO"))
  (assert-true (jlog::level> "INFO" "WARNING"))
  (assert-true (jlog::level> "WARNING" "ERROR"))
  (assert-true (jlog::level> "ERROR" "CRITICAL"))
  ;;
  (assert-false (jlog::level> "INFO" "DEBUG"))
  (assert-false (jlog::level> "WARNING" "INFO"))
  (assert-false (jlog::level> "ERROR" "WARNING"))
  (assert-false (jlog::level> "CRITICAL" "ERROR"))
  )

(deftest level<-test (logger-suite)
  (assert-false (jlog::level< "DEBUG" "INFO"))
  (assert-false (jlog::level< "INFO" "WARNING"))
  (assert-false (jlog::level< "WARNING" "ERROR"))
  (assert-false (jlog::level< "ERROR" "CRITICAL"))
  ;;
  (assert-true (jlog::level< "INFO" "DEBUG"))
  (assert-true (jlog::level< "WARNING" "INFO"))
  (assert-true (jlog::level< "ERROR" "WARNING"))
  (assert-true (jlog::level< "CRITICAL" "ERROR"))
  )

;; (defun log-write-output (level title string)
;; (defun get-stamp (&optional (stamp "STAMP"))

(format t "~a~%" (run-suite 'logger-suite :stop-on-fail nil :report-progress nil))
