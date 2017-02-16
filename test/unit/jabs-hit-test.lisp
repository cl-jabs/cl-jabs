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

(dolist (v '("tools" "re" "core" "cli" "project" "plugin"))
  (load (make-pathname :directory '(:relative "src") :name (concatenate 'string "jabs-" v) :type "lisp")))

(defvar jabs::*jabs-rounds-to-run* nil)

(dolist (v '("skeleton" "hit"))
  (eval `(load (make-pathname :directory '(:relative "src" "core"
                                           ,(concatenate 'string "jabs-" v))
                              :name ,(concatenate 'string "jabs-" v) :type "lisp"))))

(defpackage hit-test
  (:use :cl :test-engine :clunit :jabs :hit@core@plugin@jabs))

(in-package :hit-test)

;; tests
(defsuite hit-suite ())

;; (deftest register-hit-test (hit-suite)
;;   (assert-true (jabs::register-hit :test :testround))
;;   (assert-condition type-error (jabs::register-hit '(:test) :testround)))

(defsuite filled-hit-suite (hit-suite))

(deffixture filled-hit-suite (@body)
  ;; (flet ((jabs::run-round (&rest x)
	;; 		  (declare (ignore x))
	;; 		  t))
	;; (let ((jabs::*jabs-hit-registry* (make-hash-table))
	;;       (jabs::*jabs-project-registry* (make-hash-table)))
	;;   ;;
	;;   (jabs::register-hit :just-mock :testround :testround-2)
	;;   ;;
  (let ((jabs::*jabs-hit-registry* (make-hash-table)))
    (defproject :just-mock-project
        :name "JUST-MOCK"
        :version "0.0.1"
        :hit :just-mock
        :components ((:file "test")))
    (defhit :mock-hit () (progn (format nil "T") t))

	  @body))

;; (defvar *jabs-hit-registry* (make-hash-table :test 'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest jabs-hit-registry-test (hit-suite)
  (assert-equal 'hash-table (type-of jabs::*jabs-hit-registry*)))

(deftest find-hit-test (filled-hit-suite)
  (assert-true (find-hit :mock-hit))
  (assert-false (find-hit :mock-hit-unreg)))

(deftest register-hit-test (filled-hit-suite)
  (assert-true (jabs::register-hit :mock-hit () '(format nil "test")))
  (assert-condition type-error (jabs::register-hit :mock-hit () :mocksymbol))
  (assert-condition type-error (jabs::register-hit "mock-hit" () '(format nil "test")))
  (assert-condition type-error (jabs::register-hit nil () '(format nil "test")))
  (assert-condition type-error (jabs::register-hit :mock-hit :mockdep '(format nil "test")))
  )

;; (defun check-hit-dependencies (name &key parent-hits)

;; (defun find-hit-file (name)

;; (defun parse-hit-from-file (file)

(deftest run-hit-test (filled-hit-suite)
  (let ((jabs::*jabs-current-project* (find-project :just-mock-project)))
    (assert-equal t (run-hit :mock-hit))))

(format t "~a~%" (run-suite 'hit-suite :stop-on-fail nil :report-progress nil))
