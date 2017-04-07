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
(defpackage package@core@plugin@jabs
  (:use :cl :tools@jabs :re@jabs :jabs
	:skeleton@core@plugin@jabs))

(in-package :package@core@plugin@jabs)

(make-instance 'jabs::plugin :name :package :type :core :version jabs::+jabs-version+)

(define-plugin-type :package plugin
  (let ((checker t)
        (plugin-package-name
         (eval (append '(concat-keywords-w-delimiter *jabs-universal-delimiter*)
                       (list (get-plugin-name plugin))
                       (list (get-plugin-type plugin))
                       jabs::+jabs-plugin-namespace+))))
    ;; TODO: check for functions format (args and their types)
    (dolist (v '(:make-package))
      (when (not (try (symbol-function (tosymbol v plugin-package-name))))
        (setf checker nil)))
    checker))

(bind-project-symbol
 :package
 #'(lambda (x)
     (check-type x list)
     (let ((name (cadr (member :name x)))
	   (types (cadr (member :types x)))
	   (version (cadr (member :version x))))
       (format nil "~a ~a ~a" name types version))))

(defgeneric pack-project ( project name type version components)
  )

(defmethod pack-project ((project project) name type version components)
  "Check if project can be reachable in all accessible plugins"
  (let ((plugins (filter-project-plugins-by-type project :repository)))
    (plugins-api-call-to-true
     :make-package plugins name type version components)))
