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
(in-package :quicklisp@repository@plugin@jabs)

;;; API

(defun find-repository-project (name)
  (funcall (tools@jabs:tosymbol :find-system :ql-dist) name))

(defun load-repository-project (name)
  (jlog:info "Loading package ``~a''" name)
  (funcall (tools@jabs:tosymbol :quickload :ql) name))

(defun remove-repository-project (name)
  (funcall (tools@jabs:tosymbol :uninstall :ql) name))

(defun repository-project-version (name)
  (slot-value
   (slot-value
    (find-repository-project name)
    (tools@jabs:tosymbol :release :ql-dist))
   (tools@jabs:tosymbol :prefix :ql-dist)))

(defun update-repository-project-list ()
  (funcall (tools@jabs:tosymbol :update-all-dists :ql)))

(defun repository-project-dependencies (name)
  (let ((project (find-repository-project name)))
    (if project
        (mapcar #'tools@jabs:tosymbol (slot-value project (tools@jabs:tosymbol :required-systems :ql-dist)))
        (jlog:crit "There is no project, named ``~a'' in quickslisp. Can not resolve dependencies" name))))
