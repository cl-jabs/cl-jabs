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

(dolist (v '("skeleton" "hit" "round"))
  (eval `(load (make-pathname :directory '(:relative "src" "core"
                                           ,(concatenate 'string "jabs-" v))
                              :name ,(concatenate 'string "jabs-" v) :type "lisp"))))

(defpackage round-test
  (:use :cl :test-engine :clunit :jabs :round@core@plugin@jabs))

(in-package :round-test)

;; tests
(defsuite round-suite ())

;; (deftest register-round-test (round-suite)
;;   (assert-true (jabs::register-round :test :testround))
;;   (assert-condition type-error (jabs::register-round '(:test) :testround)))

(defsuite filled-round-suite (round-suite))

(deffixture filled-round-suite (@body)
  ;; (flet ((jabs::run-round (&rest x)
	;; 		  (declare (ignore x))
	;; 		  t))
	;; (let ((jabs::*jabs-round-registry* (make-hash-table))
	;;       (jabs::*jabs-project-registry* (make-hash-table)))
	;;   ;;
	;;   (jabs::register-round :just-mock :testround :testround-2)
	;;   ;;
  (let ((jabs::*jabs-round-registry* (make-hash-table)))
    (defproject :just-mock-project
        :name "JUST-MOCK"
        :version "0.0.1"
        :round :just-mock
        :components ((:file "test")))
    (defround :mock-round :hit1 :hit2)

	  @body))

;; (defvar *jabs-round-registry* (make-hash-table :test 'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest jabs-round-registry-test (round-suite)
  (assert-equal 'hash-table (type-of jabs::*jabs-round-registry*)))

(deftest find-round-test (filled-round-suite)
  (assert-true (find-round :mock-round))
  (assert-false (find-round :mock-round-unreg)))

(deftest register-round-test (filled-round-suite) ;; FIXME: handle crit with condition, when note just fail
  (assert-true (jabs::register-round :mock-round :hit1 :hit2))
  )

(deftest find-round-file-test (filled-round-suite)
  (assert-false (jabs::find-round-file :mock-round))) ; TODO: stub

;; (defun parse-round-from-file (file) ;; TODO: stub

(deftest run-round-test (filled-round-suite) ; TODO: mock run-hit function
  (let ((jabs::*jabs-current-project* (find-project :just-mock-project)))
    (assert-equal t (run-round :mock-round))))

(format t "~a~%" (run-suite 'round-suite :stop-on-fail nil :report-progress nil))
