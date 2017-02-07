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

(load (make-pathname :directory '(:relative "src" ) :name "jabs-re" :type "lisp"))
(load (make-pathname :directory '(:relative "src" ) :name "jabs-tools" :type "lisp"))
(load (make-pathname :directory '(:relative "src" ) :name "jabs-core" :type "lisp"))

(defpackage bind-symbols-test
  (:use :cl :clunit))

(in-package :bind-symbols-test)


(load (make-pathname :directory '(:relative "src" ) :name "jabs-project" :type "lisp"))

(setf clunit:*clunit-report-format* :tap)

(defsuite bind-symbols-suite ()
  )

;; True (as symbol)
(deftest bind-symbols-test-symbol (bind-symbols-suite)
  (let ((symbol :test))
    (assert-true (jabs::bind-project-symbol symbol #'(lambda(&rest x) (declare (ignore x)) t)))
    (assert-true (gethash (symbol-name symbol) jabs::*jabs-project-bound-symbols*))
    ))

;; False (as string)
(deftest bind-symbols-test-string (bind-symbols-suite)
  (let ((symbol "test"))
    (assert-false (ignore-errors
                    (jabs::bind-project-symbol
                     symbol
                     #'(lambda (&rest x) (declare (ignore x)) t)))) ;; not working with assert-fail for some reason. ignore-errors needed
    (assert-false (gethash symbol jabs::*jabs-project-bound-symbols*))
    ))

(format t "~a~%" (run-suite 'bind-symbols-suite :stop-on-fail nil :report-progress nil))
