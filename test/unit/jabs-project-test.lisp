;; (defvar *jabs-project-registry* (make-hash-table))
;; (defvar *jabs-project-to-run* nil) ;; when null, run all projects in registry
;; (defvar *jabs-current-project* nil)
;; (defvar *jabs-add-project* nil)
;; (defvar *define-project-hook* nil
;; (defvar *pre-run-project-hook* nil
;; (defvar *run-project-hook* nil
;; (defvar *post-run-project-hook* nil
;; (defun find-project (name)
;; (defvar *jabs-project-bound-symbols* (make-hash-table :test 'equal)
;; (defun bind-project-symbol (project-symbol function)
;; (defun register-project (name args)
;; (defmethod run-project ((project project))
;; (defmethod project-slot-value ((project project) slot)
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
(load (make-pathname :directory '(:relative "src") :name "jabs-tools" :type "lisp"))
(load (make-pathname :directory '(:relative "src") :name "jabs-core" :type "lisp"))
(load (make-pathname :directory '(:relative "src") :name "jabs-cli" :type "lisp"))
(load (make-pathname :directory '(:relative "src") :name "jabs-project" :type "lisp"))

(defpackage project-test
  (:use :cl :test-engine :clunit :jabs))

(in-package :project-test)

(setf jlog:*log-level* "ERROR")
(setf jlog:*fail-on-error* nil)
(setf jlog:*fail-on-critical* nil)
(setf jlog:*log-disable-notes* t)

;; tests
(defsuite project-suite ()
  )

(defvar *project-exists-name* :exists)
(defvar *project-exists-version* "0.0.0")
(defvar *project-not-exists-name* :not-exists)
(defvar *project-symbols-list* '(:name :long-name :description :long-description
				       :license :repositories :version
				       :pathname :depends-on :author :maintainer
				       :homepage :bug-tracker :mailto :source-control
				       :serial :bout :components :hits :source))

(dolist (v *project-symbols-list*)
  (bind-project-symbol v #'(lambda (&rest x) (declare (ignore x)) t)))

(defproject :just-mock
  :name "JUST-MOCK"
  :version *project-exists-version*
  :components ((:file "test")))

(defproject *project-exists-name*
  :name *project-exists-name*
  :version *project-exists-version*
  :components ((:file "test")))

(defvar *exists-project* (gethash *project-exists-name* jabs::*jabs-project-registry*))

(deftest find-project-test (project-suite)
  (assert-true (eq (find-project *project-exists-name*) *exists-project*))
  (assert-false (find-project *project-not-exists-name*)))

(deftest register-project-test (project-suite)
  (assert-true (jabs::register-project :just-mock-2 '(:name "JUST-MOCK-2" :components ((:file "test")))))
  (assert-true (find-project :just-mock-2))
  (assert-true (jabs::register-project :just-mock-2 '(:name "JUST-MOCK-2" :components ((:file "test"))))))


(deftest defproject-test (project-suite)
  (assert-true (defproject :just-mock-3 :name "JUST-MOCK-2" :components ((:file "test"))))
  (assert-true (find-project :just-mock-3))
  (assert-true (defproject :just-mock-3 :name "JUST-MOCK-2" :components ((:file "test")))))

(deftest run-project-test (project-suite)
  (assert-true (jabs::run-project (find-project *project-exists-name*))))

(format t "~a~%" (run-suite 'project-suite :stop-on-fail nil :report-progress nil))
