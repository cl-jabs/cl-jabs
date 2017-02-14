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

(load (make-pathname :directory '(:relative "src") :name "jabs-tools" :type "lisp"))
(load (make-pathname :directory '(:relative "src") :name "jabs-re" :type "lisp"))
(load (make-pathname :directory '(:relative "src") :name "jabs-core" :type "lisp"))

(defpackage core-test
  (:use :cl :test-engine :clunit :jabs))

(in-package :core-test)

;; tests
(defsuite core-suite ())
(defsuite hooks-core-suite (core-suite))

(deffixture hooks-core-suite (@body)
  (let (4add-hook
        4run-hook)
    (add-hook 4add-hook #'(lambda () (format nil "test")))
    (add-hook 4run-hook #'(lambda (x y) (+ x y)))
    @body))

;; :add-hook
(deftest add-hook-test (hooks-core-suite)
  (assert-equal "test" (funcall (car 4add-hook))))

;; :run-hook
(deftest run-hook-test (hooks-core-suite)
  (assert-false ;; TODO uses 'dolist' now. Make possibility to collect output data
   (jabs::run-hook 4run-hook 1 2)))

(deftest jabs-universal-delimiter-test (core-suite)
  (assert-equal "@" jabs:*jabs-universal-delimiter*))

;; (defun load-build-jab (dir)

(deftest get-option-suffix-test (core-suite)
  (assert-equal 5 (jabs::get-option-suffix :test '(1 2 3 :test 5 :zzz)))
  (assert-condition type-error (jabs::get-option-suffix :test :zzz)))


(deftest remove-with-arg-test (core-suite)
  (assert-equal '(1 2 3 :zzz) (jabs::remove-with-arg :test '(1 2 3 :test 5 :zzz)))
  (assert-condition type-error (jabs::remove-with-arg :test :zzz)))

(format t "~a~%" (run-suite 'core-suite :stop-on-fail nil :report-progress nil))
