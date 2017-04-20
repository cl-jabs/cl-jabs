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

(in-package :tgz@archive@plugin@jabs)

(defvar *archive-extension* "tgz")

;; (export '(:make-project-archive))

(defun remove-car (list1 list2)
  (cond ((null list1) list2)
        ((equal (car list1) (car list2))
         (remove-car (cdr list1) (cdr list2)))
        (t nil)))

(defmethod pathname-convert-to-relative ((pathname pathname))
  (let ((pathname-directory (pathname-directory pathname))
        (here (cdr (pathname-directory (os-pwd)))))
    (if (eq (car pathname-directory) :relative)
        pathname
        (eval `(make-pathname :directory ',(cons :relative (remove-car here (cdr pathname-directory)))
                              :name ,(pathname-name pathname)
                              :type ,(pathname-type pathname))))))

(defun make-project-archive (name version source)
  ;; change directory to source
  (os-cd source)
  (let* ((archive-name (concatenate 'string (tostr name t) "-" (tostr version t)))
         (archive-file (concatenate 'string archive-name "." *archive-extension*))
         (filelist (os-find source :type :file))
         (here (os-pwd)))
    ;; change directory to source and find files with paths, related to source dir

    (dolist (file filelist)
      (let ((new-file
             (merge-pathnames
              (pathname-convert-to-relative file)
              (make-pathname :directory (list :relative archive-name)))))
        (os-mkdir (make-pathname :directory (pathname-directory new-file)))
        (os-cp file new-file :recursive t :force t)
        ))
    ;; create archive
    (archive::create-tar-file (merge-pathnames
                               (parse-namestring archive-file)
                               (pathname-as-directory source))
                              (mapcar #'(lambda (x)
                                          (merge-pathnames
                                           (pathname-convert-to-relative x)
                                           (make-pathname :directory (list :relative archive-name))))
                                      filelist))
    ;; remove working directory
    (os-rm archive-name :recursive t)))
