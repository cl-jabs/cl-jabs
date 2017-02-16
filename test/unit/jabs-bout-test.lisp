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

(dolist (v '("skeleton" "hit" "round" "bout"))
  (eval `(load (make-pathname :directory '(:relative "src" "core"
                                           ,(concatenate 'string "jabs-" v))
                              :name ,(concatenate 'string "jabs-" v) :type "lisp"))))

(defpackage bout-test
  (:use :cl :test-engine :clunit :jabs :bout@core@plugin@jabs))

(in-package :bout-test)

;; tests
(defsuite bout-suite ())

(deftest register-bout-test (bout-suite)
  (assert-true (jabs::register-bout :test :testround))
  (assert-condition type-error (jabs::register-bout '(:test) :testround)))

(defsuite filled-bout-suite (bout-suite))

(deffixture filled-bout-suite (@body)
  (flet ((jabs::run-round (&rest x)
			  (declare (ignore x))
			  t))
	(let ((jabs::*jabs-bout-registry* (make-hash-table))
	      (jabs::*jabs-project-registry* (make-hash-table)))
	  ;;
	  (jabs::register-bout :just-mock :testround :testround-2)
	  ;;
	  (defproject :just-mock-project
	    :name "JUST-MOCK"
	    :version "0.0.1"
	    :bout :just-mock
	    :components ((:file "test")))
	  (let ((jabs::*jabs-current-project* (find-project :just-mock-project)))
	  @body))))

;; (defvar *jabs-bout-registry* (make-hash-table :test 'equal))


(deftest find-bout-test (filled-bout-suite)
  (assert-true (find-bout :just-mock))
  (assert-false (find-bout :just-mock-2))
  (assert-condition type-error (find-bout "just-mock")))

(deftest run-bout-test (filled-bout-suite)
  (let ((project (find-project :just-mock-project)))
    (assert-true (jabs::run-bout project :just-mock))
    (assert-true (jabs::run-bout project :just-mock :testround))))

;; (deftest parse-bout-from-file-test (bout-suite)
;;   (assert-equal t (parse-bout-from-file file)))

(deftest find-bout-file-test (bout-suite)
  ;; TODO: make positive test
  (assert-equal 'pathname (type-of (jabs::find-bout-file :default))) ;; FIXME: stub real pathnames
  (assert-false (jabs::find-bout-file :test))
  (assert-condition type-error (jabs::find-bout-file "test")))

;; (deftest load-bouts-by-name-test (filled-bout-suite)
;;   (assert-equal t (load-bouts-by-name (project project) bout-names)))

;; (deftest load-project-bouts-test (filled-bout-suite) ;; depends on load-bouts-by-name > parse-bout-from-file
;;   (let ((proj (find-project :just-mock-project)))
;;     (assert-equal t (jabs::load-project-bouts proj))))

(deftest insert-round-test (filled-bout-suite)
  (assert-equal '(:mockround-begin :testround :testround-2) (jabs::insert-round :mockround-begin :just-mock)))

(deftest append-round (filled-bout-suite)
  (assert-equal '(:testround :testround-2 :mockround-end) (jabs::append-round :mockround-end :just-mock)))

(deftest delete-round (filled-bout-suite)
  (assert-equal '(:testround-2) (jabs::delete-round :testround :just-mock)))

(deftest run-project-bouts (filled-bout-suite)
  (let ((proj (find-project :just-mock-project)))
    (assert-equal t (jabs::run-project-bouts proj))))

(format t "~a~%" (run-suite 'bout-suite :stop-on-fail nil :report-progress nil))

