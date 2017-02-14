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

(dolist (v '("skeleton" "asdf"))
  (eval `(load (make-pathname :directory '(:relative "src" "core"
                                           ,(concatenate 'string "jabs-" v))
                              :name ,(concatenate 'string "jabs-" v) :type "lisp"))))

(defpackage asdf-test
  (:use :cl :test-engine :clunit :jabs :asdf@core@plugin@jabs))

(in-package :asdf-test)

;; tests
(defsuite asdf-suite ())
(defsuite bind-asdf-suite (asdf-suite))

;; (defvar *asdf-symbols-list* '(:name :long-name :description :long-description
;; (defvar *required-asdf-version* "3.1")

(deftest asdf-system-structure-test (asdf-suite)
  (assert-equal 'hash-table (type-of *asdf-system-structures*)))


;; (defun dumb-string-hash (string) ;; FIXME: WTF

(deftest asdf-fasl-pathname-test (asdf-suite)
  (let ((*jabs-source-directory* "/tmp/"))
    (assert-equal (parse-namestring "/tmp/asdf.fasl") (asdf@core@plugin@jabs::asdf-fasl-pathname))))

;; (defun asdf-fasl-pathname ()

;; (defun ensure-asdf-loaded ()

;; (defmethod define-asdf-system ((project project))

;; (defmethod set-additional-sources ((project project))

;; (defun define-dummy-project (name)

(format t "~a~%" (run-suite 'asdf-suite :stop-on-fail nil :report-progress nil))
