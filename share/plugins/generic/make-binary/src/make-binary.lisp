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
(in-package :make-binary@generic@plugin@jabs)

(defvar *make-binary-toplevel-function* nil)
(defvar *make-binary-name* nil)

(defbout :make-binary
  :validate
  :initialize
  :compile
  :make-binary
  )

(defround :make-binary :make-binary)

;; (append-hit :make-binary :compile)

(defhit make-binary () ()
        (if *make-binary-toplevel-function*
            (let* ((toplevel-list (re@jabs:split #\: *make-binary-toplevel-function*))
                   (package (or (find-package (tools@jabs:tokeyword (first toplevel-list))) *package*))
                   (function (find-symbol (string-upcase (car (last toplevel-list))) package))
                   (name (or *make-binary-name*
                             (tostr (get-project-name *jabs-current-project*) t)))
                   (folder (or
                            (pathname-as-directory
                             (parse-namestring
                              (get-skeleton-bin
                               (find-project-skeleton *jabs-current-project*))))
                            "")))
              ;;
              (when (not function)
                (jlog:crit "Can not find symbol, named ``~a'' for top-level function. Can not create binary" *make-binary-toplevel-function*))
              ;;
              (tools@jabs:os-mkdir folder)
              ;;
              (trivial-dump-core:save-executable
               (merge-pathnames
                (make-pathname :name name)
                folder)
               function)
              )
            (jlog:crit "There is no toplevel function defined. Can not create binary")))

(bind-project-symbol
 :make-binary
 #'(lambda (x)
     (check-type x list)
     (let ((name (cadr (member :name x)))
           (toplevel-function (cadr (member :toplevel-function x))))
       ;;
       (when (not *make-binary-name*)
         (setf *make-binary-name* name))
       (when (not *make-binary-toplevel-function*)
         (setf *make-binary-toplevel-function* toplevel-function)))))

(bind-jabs-cli-parameter
 "make-binary-name"
 #'(lambda (x)
     (setf *make-binary-name* x)))

(bind-jabs-cli-parameter
 "make-binary-toplevel-function"
 #'(lambda (x)
     (setf *make-binary-toplevel-function* x)))

(process-jabs-cli-parameter "make-binary-name")
(process-jabs-cli-parameter "make-binary-toplevel-function")
