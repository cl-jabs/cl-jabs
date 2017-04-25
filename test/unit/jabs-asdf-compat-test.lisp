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

(dolist (v '("skeleton" "asdf-compat"))
  (eval `(load (make-pathname :directory '(:relative "src" "core"
                                           ,(concatenate 'string "jabs-" v))
                              :name ,(concatenate 'string "jabs-" v) :type "lisp"))))

(defpackage asdf-test
  (:use :cl :test-engine :clunit :jabs :asdf-compat@core@plugin@jabs))

(in-package :asdf-test)

;; tests
(defsuite asdf-suite ())
(defsuite asdf-suite-with-project (asdf-suite))
(defsuite asdf-suite-with-system (asdf-suite))

(deffixture asdf-suite-with-project (@body)
  (let ((jabs::*jabs-project-registry* (make-hash-table)))
    ;; define default project in isolated area
    (defproject :just-mock
        :name "JUST-MOCK"
        :version "0.0.1"
        :components ((:file "test")))))

(deffixture asdf-suite-with-system (@body)
  (asdf:defsystem :just-mock-system
    :name "JUST-MOCK-SYSTEM"
    :version "0.0.1"
    :components ((:file "test"))))

(dolist (v '(:name :long-name :description :long-description
             :weakly-depends-on :depends-on :class
             :build-operation
             :license :repositories :version
             :pathname :author :maintainer
             :default-component-class :perform :explain
             :output-files :operation-done-p :if-feature
             :in-order-to
             :homepage :bug-tracker :mailto :source-control
             :serial :components :source))
  (eval `(deftest ,(intern (concatenate 'string "ASDF-SYMBOLS-CHECK-" (princ-to-string v))) (asdf-suite)
           (assert-true (member ,v asdf-compat@core@plugin@jabs::*asdf-symbols-list*)))))

(deftest asdf-system-structure-test (asdf-suite)
  (assert-equal 'hash-table (type-of asdf-compat@core@plugin@jabs::*asdf-system-structures*)))

;; (defun dumb-string-hash (string) ;; FIXME: WTF

(deftest asdf-fasl-pathname-test (asdf-suite)
  (let ((*jabs-source-directory* "/tmp/"))
    (assert-equal (parse-namestring "/tmp/asdf.fasl") (asdf-compat@core@plugin@jabs::asdf-fasl-pathname))))

;; (defun ensure-asdf-loaded ()

(deftest define-asdf-system-test (asdf-suite-with-project)
  (let ((proj (find-project :just-mock)))
    (assert-equal :zzz (define-asdf-system proj))))

(deftest define-dummy-project-test (asdf-suite-with-system)
  (assert-equal :zzz (asdf-compat@core@plugin@jabs::define-dummy-project :dummy)))

;; (defun define-dummy-project (name)

(format t "~a~%" (run-suite 'asdf-suite :stop-on-fail nil :report-progress nil))
