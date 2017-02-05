(in-package #:tmpl@generic@plugin@jabs)

(defvar *tmpl-plugin-type* "generic")
;; (defvar *tmpl-author* :default)
;; (defvar *tmpl-version* "0.0.1")

(defbout :tmpl-mkplugin :tmpl-mkplugin)
(defround :tmpl-mkplugin :tmpl-mkplugin)

(insert-hit :initialize :tmpl-mkplugin)

(defun generate-plugin-build-template (name &key
                                              (type "generic")
                                              (author "John Doe <john@doe.local>")
                                              (version "0.0.1")
                                              (maintainer "John Doe <john@doe.local>")
                                              (license "Public Domain")
                                              (description "Personal use plugin")
                                              (skeleton "(:default)")
                                              (bout "default")
                                              (plugins "(:quicklisp@repository)")
                                              (components (format nil "~15t(:file \"fixture\")~%")))
  (format nil ";;; -*- Mode: Lisp -*-
(defpackage :~a-system
  (:use :cl :jabs))

(in-package :~a-system)

(defplugin ~a :~a \"~a\"
  :author \"~a\"
  :maintainer \"~a\"
  :license \"~a\"
  :description \"~a\"
  :serial t
  :skeleton ~a
  ;; :bout :~a
  ;; :plugins ~a
  ;;;; Additional sources
  ;; :sources ((make-pathname :directory '(:relative \"another-lib\")))
  ;;;; Dependencies
  ;; :depends-on (:alexandria :cl-ppcre)
  ;; :pathname \"some/relative/path\" ;; relative name to your plugin root directory (where skeleton located)
  ;;;; Add files, mobules etc
  :components (
~a))
" name name name type version author maintainer license description skeleton bout plugins components))

(defhit tmpl-mkplugin () ()
        (let* ((plugin-name (get-project-name *jabs-current-project*))
               (plugin-description (get-project-description *jabs-current-project*))
               (plugin-skeleton-name (car (slot-value *jabs-current-project* 'jabs::skeleton)))
               (plugin-components (slot-value *jabs-current-project* 'jabs::components))
               (plugin-component-names)
               (plugin-template))
          ;; try to expand to files
          (if plugin-components
              (dolist (f plugin-components)
                ;; HACK: our "components" from template are only files
                (setf plugin-component-names
                      (concatenate 'string plugin-component-names
                                   (format nil (concatenate 'string "~15t(:file \"" (cadr f) "\")~%"))))
                )
              (setf plugin-component-names (format nil "~15t(:file \"fixture\")~%")))
          ;; generate build.jab
          (setf plugin-template
                (generate-plugin-build-template
                 (string-downcase (princ-to-string (tools@jabs:tosymbol plugin-name)))
                 :type *tmpl-plugin-type*
                 :author (ignore-errors (slot-value *jabs-current-project* 'jabs::author))
                 :version (ignore-errors (slot-value *jabs-current-project* 'jabs::version))
                 :maintainer (ignore-errors (slot-value *jabs-current-project* 'jabs::maintainer))
                 :license (ignore-errors (slot-value *jabs-current-project* 'jabs::license))
                 :description plugin-description
                 :plugins (ignore-errors (slot-value *jabs-current-project* 'jabs::plugins))
                 :skeleton (concatenate 'string "(:" (string-downcase (princ-to-string plugin-skeleton-name)) ")")
                 :components plugin-component-names
                 ))
          ;;
          (jlog:note "Buildfile is ``~a''" *jabs-buildfile*)
          (os-touch *jabs-buildfile*)
          (jlog:note "Generating buildfile ``~a''..." *jabs-buildfile*)
          (with-open-file
              (bf *jabs-buildfile* :direction :output :if-exists :overwrite :if-does-not-exist :create)
            (format bf plugin-template))))

;; no plugins to run, so run one
(add-hook
    *post-init-hook*
  (lambda ()
    (if (file-exists-p (merge-pathnames (make-pathname :name "build" :type "jab") (os-pwd)))
	(progn
	  (jlog:note "Plugin already exists. Nothing to create")
	  (terminate 0))
        (progn
          (jlog:note "Plugin not exists. Going on")
          (load-skeleton (list *tmpl-skeleton-name*))
          ;;
          (let* ((plugin-name (tools@jabs:tosymbol (car (reverse (pathname-directory (os-pwd))))))
                 (plugin-type (tools@jabs:tosymbol *tmpl-plugin-type*))
                 (plugin-author "John Doe <john@doe.local>")
                 (plugin-version *tmpl-version*)
                 (plugin-maintainer "John Doe <john@doe.local>")
                 (plugin-license "Public Domain")
                 (plugin-description "Personal use plugin")
                 (plugin-serial t)
                 (plugin-plugins '(:quicklisp@repository))
                 (plugin-skeleton (find-skeleton *tmpl-skeleton-name*))
                 (plugin-bout :default)
                 (plugin-src-dir
                  (if (listp (get-skeleton-src plugin-skeleton))
                      (merge-pathnames (make-pathname :directory (list :relative (car (get-skeleton-src plugin-skeleton)))) (os-pwd))
                      (merge-pathnames (make-pathname :directory (list :relative (get-skeleton-src plugin-skeleton))) (os-pwd))))
                 (plugin-components (list-directory (merge-pathnames
                                                     plugin-src-dir
                                                     (os-pwd))))
                 (plugin-component-names))
            ;; try to expand to files
            (if plugin-components
                (dolist (f plugin-components)
                  (when (or (string-equal (pathname-type f) "lisp")
                            (string-equal (pathname-type f) "cl"))
                    (push (list :file (pathname-name f)) plugin-component-names)))
                (setf plugin-component-names '((:file "fixture"))))
            (eval
             `(defplugin ,(tools@jabs:tosymbol plugin-name) ,plugin-type ,plugin-version
                :author ,plugin-author
                :maintainer ,plugin-maintainer
                :license ,plugin-license
                :description ,plugin-description
                :serial t
                :skeleton (,(get-skeleton-name plugin-skeleton))
                :components ,(reverse plugin-component-names)))
            ;; (run-project (find-plugin (tools@jabs:tosymbol plugin-name)))
            )))))


(bind-jabs-cli-parameter
 "tmpl-plugin-type"
 #'(lambda (&rest x)
     (setf *tmpl-plugin-type* (tokeyword (car x)))))

(process-jabs-cli-parameter "tmpl-plugin-type")
