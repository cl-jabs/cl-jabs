(in-package #:swank@generic@plugin@jabs)

(defvar *swank-port* "4005")

;; (defvar *tmpl-author* :default)
;; (defvar *tmpl-version* "0.0.1")

(defhit :swank-load () ()
  (jlog:note "HERE WE GO WITH SWANK. PORT: ~a" *swank-port*))

(defround :swank-load :swank-load)

(defbout :swank-load-with-dev-server
  :validate
  :compile
  :swank-load)

;; (defun generate-build-template (name &key
;;                                        (author "John Doe <john@doe.local>")
;;                                        (version "0.0.1")
;;                                        (maintainer "John Doe <john@doe.local>")
;;                                        (license "Public Domain")
;;                                        (description "Personal use project")
;;                                        (skelethon "(:default)")
;;                                        (bout ":default")
;;                                        (plugins "(:quicklisp@repository)")
;;                                        (components (format nil "~15t(:file \"fixture\")~%")))
;;   (format nil ";;; -*- Mode: Lisp -*-
;; (defpackage :~a-system
;;   (:use :cl :jabs))

;; (in-package :~a-system)

;; (defproject :~a
;;   :name \"~a\"
;;   :author \"~a\"
;;   :version \"~a\"
;;   :maintainer \"~a\"
;;   :license \"~a\"
;;   :description \"~a\"
;;   :serial t
;;   :skelethon ~a
;;   :bout ~a
;;   ;; :plugins ~a
;;   ;;;; Additional sources
;;   ;; :sources ((make-pathname :directory '(:relative \"another-lib\")))
;;   ;; :repositories (:quicklisp) ;; define some repositories here
;;   ;;;; Dependencies
;;   ;; :depends-on (:alexandria :cl-ppcre)
;;   ;; :pathname \"some/relative/path\" ;; relative name to your project root directory (where skelethon located)
;;   ;;;; Add files, mobules etc
;;   :components (
;; ~a))
;; " name name name name author version maintainer license description skelethon bout plugins components))

;; (defhit tmpl-mkproject () ()
;;         (let* ((project-name (get-project-name *jabs-current-project*))
;;                (project-description (get-project-description *jabs-current-project*))
;;                (project-skelethon-name (car (slot-value *jabs-current-project* 'jabs::skelethon)))
;;                (project-components (slot-value *jabs-current-project* 'jabs::components))
;;                (project-component-names)
;;                (project-template))
;;           ;; try to expand to files
;;           (if project-components
;;               (dolist (f project-components)
;;                 ;; HACK: our "components" from template are only files
;;                 (setf project-component-names
;;                       (concatenate 'string project-component-names
;;                                    (format nil (concatenate 'string "~15t(:file \"" (cadr f) "\")~%"))))
;;                 )
;;               (setf project-component-names (format nil "~15t(:file \"fixture\")~%")))
;;           ;; generate build.jab
;;           (setf project-template
;;                 (generate-build-template
;;                  (string-downcase (princ-to-string (tools@jabs:tosymbol project-name)))
;;                  :author (ignore-errors (slot-value *jabs-current-project* 'jabs::author))
;;                  :version (ignore-errors (slot-value *jabs-current-project* 'jabs::version))
;;                  :maintainer (ignore-errors (slot-value *jabs-current-project* 'jabs::maintainer))
;;                  :license (ignore-errors (slot-value *jabs-current-project* 'jabs::license))
;;                  :description project-description
;;                  :plugins (ignore-errors (slot-value *jabs-current-project* 'jabs::plugins))
;;                  :skelethon (concatenate 'string "(:" (string-downcase (princ-to-string project-skelethon-name)) ")")
;;                  :components project-component-names
;;                  ))
;;           ;;
;;           (jlog:note "Buildfile is ``~a''" *jabs-buildfile*)
;;           (os-touch *jabs-buildfile*)
;;           (jlog:note "Generating buildfile ``~a''..." *jabs-buildfile*)
;;           (with-open-file
;;               (bf *jabs-buildfile* :direction :output :if-exists :overwrite :if-does-not-exist :create)
;;             (format bf project-template))))

;; ;; no projects to run, so run one
;; (add-hook
;;  *post-init-hook*
;;   (lambda ()
;;     (if (file-exists-p (merge-pathnames (make-pathname :name "build" :type "jab") (os-pwd)))
;;         (jlog:note "Project already exists. Nothing to create")
;;         (progn
;;    (load-skelethon (list *tmpl-skelethon-name*))
;;    ;;
;;    (let* ((project-name (car (reverse (pathname-directory (os-pwd)))))
;;           (project-author "John Doe <john@doe.local>")
;;           (project-version *tmpl-version*)
;;           (project-maintainer "John Doe <john@doe.local>")
;;           (project-license "Public Domain")
;;           (project-description "Personal use project")
;;           (project-serial t)
;;           (project-plugins '(:quicklisp@repository))
;;           (project-skelethon (find-skelethon *tmpl-skelethon-name*))
;;           (project-bout :default)
;;           (project-src-dir
;;            (if (listp (get-skelethon-src project-skelethon))
;;                (merge-pathnames (make-pathname :directory (list :relative (car (get-skelethon-src project-skelethon)))) (os-pwd))
;;                (merge-pathnames (make-pathname :directory (list :relative (get-skelethon-src project-skelethon))) (os-pwd))))
;;           (project-components (list-directory (merge-pathnames
;;                                                project-src-dir
;;                                                (os-pwd))))
;;           (project-component-names))
;;      ;; try to expand to files
;;      (if project-components
;;          (dolist (f project-components)
;;            (when (or (string-equal (pathname-type f) "lisp")
;;                      (string-equal (pathname-type f) "cl"))
;;              (push (list :file (pathname-name f)) project-component-names)))
;;          (setf project-component-names '((:file "fixture"))))
;;      (eval
;;       `(defproject ,(tools@jabs:tosymbol project-name)
;;            :name ,project-name
;;            :author ,project-author
;;            :version ,project-version
;;            :maintainer ,project-maintainer
;;            :license ,project-license
;;            :description ,project-description
;;            :serial t
;;            :skelethon (,(get-skelethon-name project-skelethon))
;;            :components ,project-component-names))
;;      (run-project (find-project (tools@jabs:tosymbol project-name)))
;;      )))))

;; (bind-jabs-cli-parameter
;;  "tmpl-skelethon"
;;  #'(lambda (&rest x)
;;      (setf *tmpl-skelethon-name* (car x))))

(bind-jabs-cli-parameter
 "swank-port"
 #'(lambda (&rest x)
     (setf *swank-port* (car x))))

(process-jabs-cli-parameter "swank-port")

;; (defun load-swank (port)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
