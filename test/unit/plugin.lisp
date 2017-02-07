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

(load (make-pathname :directory '(:relative "src") :name "jabs-core" :type "lisp"))
(load (make-pathname :directory '(:relative "src") :name "jabs-plugin" :type "lisp"))

(defpackage plugin-test
  (:use :cl :test-engine :clunit :jabs))

(in-package :plugin-test)

(setf jlog:*log-level* "ERROR")
(setf jlog:*fail-on-error* nil)
(setf jlog:*fail-on-critical* nil)
(setf jlog:*log-disable-notes* t)

;; tests
(defsuite plugin-suite ()
  )

(deftest plugin-test (plugin-suite)

;; plugin-type-registered-p ;; (name)
;; register-plugin-type ;; (name)
;; make-plugin-package-name ;; (name &optional (type :generic))
;; plugin ;; ()
;; plugin-update ;; (plugin)
;; plugin-registered-p ;; (plugin)
;; load-plugin ;; (plugin)
;; find-plugin ;; (name &optional (type :generic))
;; register-plugin ;; (plugin)
;; register-plugin ;; ((plugin plugin))
;; initialize-instance ;; :after ((plugin plugin) &key)
;; plugin-registered-p ;; ((plugin plugin))
)

(format t "~a~%" (run-suite 'plugin-suite :stop-on-fail nil :report-progress nil))
