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

(load (make-pathname :directory '(:relative "src" "core" "jabs-skeleton")
                     :name "jabs-skeleton" :type "lisp"))

(defpackage skeleton-test
  (:use :cl :test-engine :clunit :jabs :skeleton@core@plugin@jabs))

(in-package :skeleton-test)

;; tests
(defsuite skeleton-suite ())

;; (deftest register-skeleton-test (skeleton-suite)
;;   (assert-true (jabs::register-skeleton :test :testround))
;;   (assert-condition type-error (jabs::register-skeleton '(:test) :testround)))

(defsuite filled-skeleton-suite (skeleton-suite))

(deffixture filled-skeleton-suite (@body)
  (flet ((jabs::run-round (&rest x)
			  (declare (ignore x))
			  t))
	(let ((jabs::*jabs-skeleton-registry* (make-hash-table))
	      (jabs::*jabs-project-registry* (make-hash-table)))
	  ;;
	  (defproject :just-mock-project
        :name "JUST-MOCK"
        :version "0.0.1"
        :skeleton :mock-skeleton
        :components ((:file "test")))

    (defskeleton :mock-skeleton
        :lib "lib"
        :doc ("doc" :required)
        :conf "conf"
        :test ("test" :required))

	  @body)))

;; (defvar *jabs-default-skeleton-name* :default)
;; (defvar *jabs-skeleton-registry* (make-hash-table))

(deftest skeletondir-p-test (skeleton-suite)
  (assert-true (jabs::skeletondir-p "dir"))
  (assert-true (jabs::skeletondir-p '("dir" :required)))
  (assert-false (jabs::skeletondir-p '("dir" :mock)))
  (assert-false (jabs::skeletondir-p :mockdir))
  )

(deftest listskeletondirs-p-test (skeleton-suite)
  (assert-true (jabs::listskeletondirs-p '("dir")))
  (assert-true (jabs::listskeletondirs-p '("dir" ("reqdir" :required))))
  (assert-false (jabs::listskeletondirs-p '("dir" ("reqdir" :mock))))
  (assert-false (jabs::listskeletondirs-p :mockdir))
  )

(deftest register-skeleton-test (skeleton-suite)
  (assert-equal t (jabs::register-skeleton :mock-skeleton :bin "bin"))
  (assert-condition type-error (jabs::register-skeleton "mock-skeleton" :bin "bin")))


;; (defun make-skeleton-directory (dir &key prefix force) ;; FIXME: rewrite for correct pathnames
;; (defun find-skeleton-file (name)
;; (defun parse-skeleton-from-file (file)

(deftest find-skeleton-test (filled-skeleton-suite)
  (assert-true (find-skeleton :mock-skeleton))
  (assert-false (find-skeleton :mock-skeleton-unreg))
  (assert-condition type-error (find-skeleton nil)))

;; (defun process-skeleton (skeleton-params)
;; (defun load-skeleton (skeleton)

(deftest get-project-skeleton-name-test (filled-skeleton-suite)
  (let ((proj (find-project :just-mock-project)))
    (assert-true (jabs::get-project-skeleton-name proj))))

;; (defmethod project-skeleton-force-p ((project project))

(deftest find-project-skeleton-test (filled-skeleton-suite)
  (let ((proj (find-project :just-mock-project)))
    (assert-true (find-project-skeleton proj))))

;; (defmethod load-project-skeleton ((project project))
;; (defmethod process-project-skeleton ((project project))

(format t "~a~%" (run-suite 'skeleton-suite :stop-on-fail nil :report-progress nil))
