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
(load (make-pathname :directory '(:relative "src") :name "jabs-cli" :type "lisp"))

(defpackage cli-test
  (:use :cl :test-engine :clunit :tools@jabs))

(in-package :cli-test)

;; tests
(defsuite cli-suite ())
(defsuite bind-cli-suite (cli-suite))

(bind-jabs-cli-parameter "process" #'(lambda (&rest x) (declare (ignore x)) t))

(deffixture bind-cli-suite (@body)
  (let (#+sbcl(sb-ext:*posix-argv* '("sbcl" "-Dprocess=t"))
        (tools@jabs::*jabs-cli-actions* (make-hash-table :test 'equal)))
    (bind-jabs-cli-parameter "process" #'(lambda (&rest x) (declare (ignore x)) t))
    @body))

(deftest jabs-cli-actions-test (cli-suite)
  (assert-equal 'hash-table (type-of tools@jabs::*jabs-cli-actions*)))

(deftest bind-jabs-cli-parameter-test (cli-suite)
  (assert-equal #'car (bind-jabs-cli-parameter "test" #'car))
  (assert-condition type-error (bind-jabs-cli-parameter :testsymbol #'car)))

(deftest process-jabs-cli-parameter-test (bind-cli-suite)
  (assert-true (process-jabs-cli-parameter "process")) ; FIXME: some shit
  (assert-condition type-error (process-jabs-cli-parameter :testsymbol)))

;; (defun process-jabs-cli-parameter (parameter)


(format t "~a~%" (run-suite 'cli-suite :stop-on-fail nil :report-progress nil))
