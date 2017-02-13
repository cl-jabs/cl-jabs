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

(load (make-pathname :directory '(:relative "src") :name "jabs-re" :type "lisp"))

(defpackage re-test
  (:use :cl :clunit :re@jabs))

(in-package :re-test)

(defsuite re-suite ()
  )

;; Check re

(deftest re-test (re-suite)
  (let ((t-s "testingstring"))
    (let ((s-to-split (split #\: "test:tset")))
      (assert-true (and
		    (string-equal "test" (car s-to-split))
		    (string-equal "tset" (cadr s-to-split)))))
    ;; replace-string
    (assert-true (string-equal (replace-string t-s "ing" "wong") "testwongstring"))
    ;; replace-string-all
    (assert-true (string-equal (replace-string-all t-s "ing" "wong") "testwongstrwong"))
    ;; replace-inside-string
    (assert-true
     (string-equal (replace-inside-string t-s "wong" #\i #\r) "testwonging"))
    ;; begin-scan
    (assert-true
     (string-equal (begin-scan "test" t-s) "test"))
    (assert-false (begin-scan "ztest" t-s))
    ;; end-scan
    (assert-true
     (string-equal (end-scan "ing" t-s) "ing"))
    (assert-false (end-scan "ings" t-s))
    ;; middle-scan
    (assert-true (string-equal (middle-scan #\i #\r t-s) "ngst"))
    (assert-false (middle-scan #\z #\r t-s))
    (assert-false (middle-scan #\r #\z t-s))
    (assert-false (middle-scan #\z #\q t-s))
    ;; begin-cut
    (assert-true
     (string-equal (begin-cut "test" t-s) "ingstring"))
    (assert-false (begin-cut "ztest" t-s))
    ;; end-cut
    (assert-true
     (string-equal (end-cut "ing" t-s) "testingstr"))
    (assert-false (end-cut "ings" t-s))
    ;; scan
    (assert-true (scan "test" t-s))
    (assert-false (scan "ztest" t-s))
    ;; TODO: concatenate-to-string-with-delimiter
    ;; TODO: parse-complex-string

    ))

(format t "~a~%" (run-suite 're-suite :stop-on-fail nil :report-progress nil))

;; (defun tolist (string)
;; (defun tostr (list)
;; (defun get-before (char list)
;; (defun get-after (char list)
;; (defun get-first-n (n list)
;; (defun cut-first-n (n list)
;; (defun split (character string)
;; (defun replace-list (list replace replace-to &key all)
;; (defun replace-inside-list (list-in replacement &optional start-symbol end-symbol)
;; (defun replace-string (string replace replace-to)
;; (defun replace-string-all (string replace replace-to)
;; (defvar *expressions* '(#\^ #\$ #\* #\| #\.))
;; (defvar *exphash* (make-hash-table))
;; (defun begin-match-list (sublist mainlist)
;; (defun begin-scan (sub string)
;; (defun middle-scan (startsymbol endsymbol string)
;; (defun scan-all-to-list (startsymbol endsymbol target-string &optional collector) ; TODO: seems, not used. Remove?
;; (defun scan (sub string)
;; (defun end-scan (sub string)
;; (defun end-cut (sub string)
;; (defun begin-cut (sub string)
;; (defun replace-inside-string (string replacement &optional start-symbol end-symbol)
;; (defun concatenate-to-string-with-delimiter (delimiter &rest strings)
;; (defun parse-complex-string (string)
