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

;; tests
(defsuite project-suite ())
(defsuite external-project-suite (project-suite))
(defsuite internal-project-suite (project-suite))

;; bind basic project symbols
(dolist (v '(:name :long-name :description :long-description
             :license :repositories :version
             :pathname :depends-on :author :maintainer
             :homepage :bug-tracker :mailto :source-control
             :serial :bout :components :hits :source))
  (bind-project-symbol v #'(lambda (&rest x) (declare (ignore x)) t)))

;; make local link to *jabs-project-registry*

(defvar *project-exists-name* :exists)
(defvar *project-exists-version* "0.0.0")
(defvar *project-not-exists-name* :not-exists)

;; disable noize
(deffixture internal-project-suite (@body)
  (let ((jlog:*log-level* "CRITICAL")
        (jlog:*fail-on-error* nil)
        (jlog:*fail-on-critical* nil)
        (jlog:*log-disable-notes* t)
        (jabs::*jabs-project-registry* (make-hash-table)))
    ;; define default project in isolated area
    (defproject :just-mock
        :name "JUST-MOCK"
        :version *project-exists-version*
        :components ((:file "test")))

    (defproject *project-exists-name*
        :name *project-exists-name*
        :version *project-exists-version*
        :components ((:file "test")))

    @body))

;;; for external project
(defproject :just-mock-2
    :name "JUST-MOCK-2"
    :version *project-exists-version*
    :components ((:file "test")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *exists-project* (gethash *project-exists-name* jabs::*jabs-project-registry*))

;; (format t "Here it is: ~a~%" *exists-project*)

(deftest jabs-project-registry-test (external-project-suite)
  (assert-eq 'hash-table (type-of jabs::*jabs-project-registry*))
  (assert-eq 1 (hash-table-count jabs::*jabs-project-registry*))) ;; FIXME: why not 2?

(deftest find-project-test (internal-project-suite)
  (assert-true (eq (find-project *project-exists-name*) *exists-project*))
  (assert-false (find-project *project-not-exists-name*)))

(deftest register-project-test (internal-project-suite)
  (assert-true (jabs::register-project :just-mock-2 '(:name "JUST-MOCK-2" :components ((:file "test")))))
  (assert-true (find-project :just-mock-2))
  (assert-true (jabs::register-project :just-mock-2 '(:name "JUST-MOCK-2" :components ((:file "test"))))))

(deftest defproject-test (internal-project-suite)
  (assert-true (defproject :just-mock-3 :name "JUST-MOCK-2" :components ((:file "test"))))
  (assert-true (find-project :just-mock-3))
  (assert-true (defproject :just-mock-3 :name "JUST-MOCK-2" :components ((:file "test")))))

(deftest run-project-test (internal-project-suite)
  (assert-false (run-project (find-project :just-mock)))) ; TODO: make run-project more informative

(deftest bind-project-symbol-test (external-project-suite)
  (assert-eq #'car (bind-project-symbol :zzz #'car))
  (assert-true (gethash :zzz jabs::*jabs-project-bound-symbols*)))

(deftest project-slot-value-test (internal-project-suite)
  (let ((proj (find-project :just-mock)))
    (assert-false (project-slot-value proj :homepage))
    (assert-false (project-slot-value proj :components))))

(format t "~a~%" (run-suite 'project-suite :stop-on-fail nil :report-progress nil))
