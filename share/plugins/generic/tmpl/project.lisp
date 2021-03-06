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

(in-package #:tmpl@generic@plugin@jabs)


(defvar *tmpl-name* (car (reverse (pathname-directory (os-pwd)))))
(defvar *tmpl-author* :default)
(defvar *tmpl-version* "0.0.1")
(defvar *tmpl-skeleton-name* :default)

(bind-jabs-cli-parameter
 "tmpl-name"
 #'(lambda (&rest x)
     (setf *tmpl-name* (car x))))

(process-jabs-cli-parameter "tmpl-name")

(defbout :tmpl-mkproject :tmpl-mkproject)
(defround :tmpl-mkproject :tmpl-mkproject)

(insert-hit :initialize :tmpl-mkproject)

(defun generate-build-template (name &key
                                       (author "John Doe <john@doe.local>")
                                       (version "0.0.1")
                                       (maintainer "John Doe <john@doe.local>")
                                       (license "Public Domain")
                                       (description "Personal use project")
                                       (skeleton "(:default)")
                                       (bout ":default")
                                       (plugins "(:quicklisp@repository)")
                                       (components (format nil "~15t(:file \"fixture\")~%")))
  (format nil ";;; -*- Mode: Lisp -*-
(defpackage :~a-system
  (:use :cl :jabs))

(in-package :~a-system)

(defproject :~a
  :name \"~a\"
  :author \"~a\"
  :version \"~a\"
  :maintainer \"~a\"
  :license \"~a\"
  :description \"~a\"
  :serial t
  :skeleton ~a
  :bout ~a
  ;; :plugins ~a
  ;;;; Dependencies
  ;; :depends-on (:alexandria :cl-ppcre)
  ;; :pathname \"some/relative/path\" ;; relative name to your project root directory (where skeleton located)
  ;;;; Add files, mobules etc
  :components (
~a))
" name name name name author version maintainer license description skeleton bout plugins components))

(defhit tmpl-mkproject () ()
        (let* ((project-name *tmpl-name*)
               (project-description (get-project-description *jabs-current-project*))
               (project-skeleton-name (car (slot-value *jabs-current-project* 'jabs::skeleton)))
               (project-components (slot-value *jabs-current-project* 'jabs::components))
               (project-component-names)
               (project-template))
          ;; try to expand to files
          (if project-components
              (dolist (f project-components)
                ;; HACK: our "components" from template are only files
                (setf project-component-names
                      (concatenate 'string project-component-names
                                   (format nil (concatenate 'string "~15t(:file \"" (cadr f) "\")~%"))))
                )
              (setf project-component-names (format nil "~15t(:file \"fixture\")~%")))
          ;; generate build.jab
          (setf project-template
                (generate-build-template
                 (string-downcase (princ-to-string (tools@jabs:tostr project-name)))
                 :author (ignore-errors (slot-value *jabs-current-project* 'jabs::author))
                 :version (ignore-errors (slot-value *jabs-current-project* 'jabs::version))
                 :maintainer (ignore-errors (slot-value *jabs-current-project* 'jabs::maintainer))
                 :license (ignore-errors (slot-value *jabs-current-project* 'jabs::license))
                 :description project-description
                 :plugins (ignore-errors (slot-value *jabs-current-project* 'jabs::plugins))
                 :skeleton (concatenate 'string "(:" (string-downcase (princ-to-string project-skeleton-name)) ")")
                 :components project-component-names
                 ))
          ;;
          (jlog:note "Buildfile is ``~a''" *jabs-buildfile*)
          (os-touch *jabs-buildfile*)
          (jlog:note "Generating buildfile ``~a''..." *jabs-buildfile*)
          (with-open-file
              (bf *jabs-buildfile* :direction :output :if-exists :overwrite :if-does-not-exist :create)
            (format bf project-template))))

;; no projects to run, so run one
(add-hook
 *post-init-hook*
 (lambda ()
   (if (file-exists-p (merge-pathnames (make-pathname :name "build" :type "jab") (os-pwd)))
       (progn
         (jlog:note "Project already exists. Nothing to create")
         (terminate 0))
       (progn
         (load-skeleton (list *tmpl-skeleton-name*))
         ;;
         (let* ((project-name *tmpl-name*)
                (project-author "John Doe <john@doe.local>")
                (project-version *tmpl-version*)
                (project-maintainer "John Doe <john@doe.local>")
                (project-license "Public Domain")
                (project-description "Personal use project")
                (project-serial t)
                (project-plugins '(:quicklisp@repository))
                (project-skeleton (find-skeleton *tmpl-skeleton-name*))
                (project-bout :default)
                (project-src-dir
                 (if (listp (get-skeleton-src project-skeleton))
                     (merge-pathnames (make-pathname :directory (list :relative (car (get-skeleton-src project-skeleton)))) (os-pwd))
                     (merge-pathnames (make-pathname :directory (list :relative (get-skeleton-src project-skeleton))) (os-pwd))))
                (project-components (list-directory (merge-pathnames
                                                     project-src-dir
                                                     (os-pwd))))
                (project-component-names))
           ;; try to expand to files
           (if project-components
               (dolist (f project-components)
                 (when (or (string-equal (pathname-type f) "lisp")
                           (string-equal (pathname-type f) "cl"))
                   (push (list :file (pathname-name f)) project-component-names)))
               (setf project-component-names '((:file "fixture"))))
           (eval
            `(defproject ,(tools@jabs:tostr project-name)
                 :name ,project-name
                 :author ,project-author
                 :version ,project-version
                 :maintainer ,project-maintainer
                 :license ,project-license
                 :description ,project-description
                 :serial t
                 :skeleton (,(get-skeleton-name project-skeleton))
                 :components ,(reverse project-component-names)))
           ;; (run-project (find-project (tools@jabs:tostr project-name)))
           )))))

(bind-jabs-cli-parameter
 "tmpl-skeleton"
 #'(lambda (&rest x)
     (setf *tmpl-skeleton-name* (tokeyword (car x)))))

(bind-jabs-cli-parameter
 "tmpl-version"
 #'(lambda (&rest x)
     (setf *tmpl-version* (car x))))

(process-jabs-cli-parameter "tmpl-skeleton")
(process-jabs-cli-parameter "tmpl-version")
