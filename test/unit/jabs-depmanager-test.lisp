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

(dolist (v '("skeleton" "depmanager"))
  (eval `(load (make-pathname :directory '(:relative "src" "core"
                                           ,(concatenate 'string "jabs-" v))
                              :name ,(concatenate 'string "jabs-" v) :type "lisp"))))

(defpackage depmanager-test
  (:use :cl :test-engine :clunit :jabs :depmanager@core@plugin@jabs))

(in-package :depmanager-test)

;; tests
(defsuite depmanager-suite ())

(deftest jabs-global-projects-map-test (depmanager-suite)
  (assert-equal 'hash-table (type-of jabs::*jabs-global-projects-map*)))

(deftest jabs-global-systems-map-test (depmanager-suite)
  (assert-equal 'hash-table (type-of jabs::*jabs-global-projects-map*)))

(deftest jabs-map-global-projects-to-files-test (depmanager-suite)
  (assert-equal nil (jabs::map-global-projects-to-files))) ; TODO: stub

(deftest jabs-map-global-systems-to-files-test (depmanager-suite)
  (assert-equal nil (jabs::map-global-systems-to-files))) ; TODO: stub

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsuite filled-depmanager-suite (depmanager-suite))

(deffixture filled-depmanager-suite (@body)
  (flet ((jabs::run-round (&rest x)
           (declare (ignore x))
           t))
    (let ((jabs::*jabs-project-registry* (make-hash-table)))
      ;;
      (defproject :just-mock-project
          :name "JUST-MOCK"
          :version "0.0.1"
          :bout :just-mock
          :depends-on (:mockdep)
          :components ((:file "test")))
      (let ((proj (find-project :just-mock-project)))
      @body))))

(deftest get-project-projects-map-test (filled-depmanager-suite)
  (assert-equal 'hash-table (type-of (jabs::get-project-projects-map proj))))

(deftest get-project-systems-map-test (filled-depmanager-suite)
  (assert-equal 'hash-table (type-of (jabs::get-project-systems-map proj))))

(deftest get-project-depends-on-test (filled-depmanager-suite)
  (assert-equal '(:mockdep) (jabs::get-project-depends-on proj)))

(deftest dependency-project-reachable-locally-p-test (filled-depmanager-suite)
  (assert-false (jabs::dependency-project-reachable-locally-p :test proj)) ; TODO: stub
  )

(deftest dependency-system-reachable-locally-p-test (filled-depmanager-suite)
  (assert-false (jabs::dependency-system-reachable-locally-p :test proj))) ; TODO: stub

(deftest find-project-dependency-force-locally-test (filled-depmanager-suite)
  (assert-equal nil (jabs::find-project-dependency-force-locally :test proj))) ; TODO: stub

(deftest dependency-project-reachable-remotely-p-test (filled-depmanager-suite)
  (assert-equal nil (jabs::dependency-project-reachable-remotely-p :test proj))) ; TODO: stub

(deftest find-project-dependencies-remotely-test (filled-depmanager-suite)
  (assert-equal nil (jabs::find-project-dependencies-remotely :test proj))) ; TODO: stub

(deftest load-project-remote-dependency-test (filled-depmanager-suite)
  (assert-equal nil (jabs::load-project-remote-dependency :test proj))) ; TODO: stub

(deftest dependency-project-reachable-p-test (filled-depmanager-suite)
  (assert-equal nil (jabs::dependency-project-reachable-p :test proj))) ; TODO: stub

(deftest make-project-depencencies-list-test (filled-depmanager-suite)
  (assert-equal nil (jabs::make-project-depencencies-list proj))) ; TODO: stub

(deftest load-project-local-dependency-test (filled-depmanager-suite)
  (assert-equal nil (jabs::load-project-local-dependency :test proj))) ; TODO: stub

(deftest load-project-dependencies-test (filled-depmanager-suite)
  (assert-equal nil (jabs::load-project-dependencies proj))) ; TODO: stub

(format t "~a~%" (run-suite 'depmanager-suite :stop-on-fail nil :report-progress nil))
