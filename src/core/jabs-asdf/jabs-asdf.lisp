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
(defpackage asdf@core@plugin@jabs
  (:use :cl :tools@jabs :re@jabs :jabs
				:skeleton@core@plugin@jabs))

;; TODO: revise https://common-lisp.net/project/asdf/asdf.html#The-defsystem-grammar
(in-package :asdf@core@plugin@jabs)

;; load local ASDF
(load (merge-pathnames
       (make-pathname :name "asdf" :type "lisp")
       *load-truename*))

(make-instance 'jabs::plugin :name :asdf :type :core :version jabs::+jabs-version+)

(require 'asdf)

;; (use-package :asdf)

;; (use-package :uiop/utility)

(defvar *asdf-symbols-list* '(:name :long-name :description :long-description
                              :weakly-depends-on :depends-on :class
                              :build-operation
                              :license :repositories :version
                              :pathname :author :maintainer
                              :default-component-class :perform :explain
                              :output-files :operation-done-p :if-feature
                              :in-order-to
                              :homepage :bug-tracker :mailto :source-control
                              :serial :components :source))

(defvar *required-asdf-version* "3.1")

(defvar *asdf-system-structures* (make-hash-table))

;; loading ASDF
;; Stolled from quicklisp yo ho ho!
(defun dumb-string-hash (string) ;; FIXME: WTF
  "Produce a six-character hash of STRING."
  (let ((hash #xD13CCD13))
    (loop for char across string
       for value = (char-code char)
       do
         (setf hash (logand #xFFFFFFFF
                            (logxor (ash hash 5)
                                    (ash hash -27)
                                    value))))
    (subseq (format nil "~(~36,6,'0R~)" (mod hash 88888901))
            0 6)))

(defun asdf-fasl-pathname ()
  "Return a pathname suitable for storing the ASDF FASL, separated
from ASDF FASLs from incompatible implementations. Also, save a file
in the directory with the implementation signature, if it doesn't
already exist."
  (let* ((implementation-signature (implementation-signature))
         (original-fasl (compile-file-pathname (merge-pathnames *jabs-source-directory* (make-pathname :name "asdf" :type "lisp"))))
         (fasl
          (merge-pathnames
           *jabs-source-directory*
           (make-pathname
            :defaults original-fasl
            :directory
            (list :relative
                  "cache"
                  "asdf-fasls"
                  (dumb-string-hash implementation-signature))))))
    (ensure-directories-exist fasl)
    fasl))

(defun ensure-asdf-loaded ()
  "Try several methods to make sure that a sufficiently-new ASDF is
loaded: first try (require 'asdf), then loading the ASDF FASL, then
compiling asdf.lisp to a FASL and then loading it."
  (let* ((source (merge-pathnames *jabs-source-directory* (make-pathname :name "asdf" :type "lisp")))
         (fasl (asdf-fasl-pathname)))
    (ensure-directories-exist fasl)
    (labels ((asdf-symbol (name)
               (let ((asdf-package (find-package '#:asdf)))
                 (when asdf-package
                   (find-symbol (string name) asdf-package))))
             (version-satisfies (version)
               (let ((vs-fun (asdf-symbol '#:version-satisfies))
                     (vfun (asdf-symbol '#:asdf-version)))
                 (when (and vs-fun vfun
                            (fboundp vs-fun)
                            (fboundp vfun))
                   (funcall vs-fun (funcall vfun) version)))))
      (block nil
        (macrolet ((try (&body asdf-loading-forms)
                     `(progn
                        (handler-bind ((warning #'muffle-warning))
                          (ignore-errors
                            ,@asdf-loading-forms))
                        (when (version-satisfies *required-asdf-version*)
                          (return t)))))
          (try)
          (try (require 'asdf))
          (try (load fasl :verbose nil))
          (try (load (compile-file source :verbose nil :output-file fasl)))
          (jlog:crit "Could not load ASDF ~S or newer" *required-asdf-version*))))))

(ensure-asdf-loaded)

;;/ Stolled from quicklisp yo ho ho ! (loading ASDF)

;; (use-package :asdf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; just dummy bindings
(dolist (v *asdf-symbols-list*)
  (bind-project-symbol v #'(lambda (&rest x) (declare (ignore x)) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defmethod asdf-build-system ((structure asdf-structure))
;;   "register ASDF system"

;;   (apply 'asdf/parse-defsystem:register-system-definition ',name
;;          (clear-defsystem ',options :prefix
;;                           (or (car (get-skeleton-src
;;                                     (find-skeleton (car (get-project-skeleton (find-project ',name))))))
;;                               "")))

(defgeneric define-asdf-system (project)
  )

(defmethod define-asdf-system ((project project))
  (let* ((asdf-list)
         (skeleton-name (or
                          (try (car (slot-value project 'skeleton)))
                          (try (slot-value project 'skeleton))
                          (get-skeleton-name (find-skeleton *jabs-default-skeleton-name*))))
         (skeleton)
         (name (get-project-name project))
         (skeleton-src))
    ;; Finding skeleton to get src-path
    (setf skeleton (find-skeleton skeleton-name))
    (let ((skeleton-got-src (get-skeleton-src skeleton)))
      (setf skeleton-src (if (atom skeleton-got-src) skeleton-got-src (car skeleton-got-src))))
    ;;
    (dolist (v *asdf-symbols-list*)
      (let ((slot (try (slot-value project (tosymbol v :jabs)))))
        (when (and slot (not (eq v :pathname)))
          (push slot asdf-list)
          (push v asdf-list))))
    (jlog:dbg "Registering ASDF system ``~a''" (get-project-name project))
    (eval `(apply
            'asdf/parse-defsystem:register-system-definition
            ,name ',(append asdf-list
                            (list :pathname
                                  (merge-pathnames
                                   (pathname-as-directory skeleton-src)
                                   (pathname-as-directory
                                    (slot-value project 'pathname)))))))))

(defgeneric set-additional-sources (project)
  )

(defmethod set-additional-sources ((project project))
  (let ((additional-sources (try (slot-value project 'jabs::sources)))
        (skel-lib
         (get-skeleton-lib
          (find-skeleton
           (or
            (ignore-errors (car (slot-value project 'jabs:skeleton)))
            (ignore-errors (slot-value project 'jabs:skeleton))
            *jabs-default-skeleton-name*)))))
    ;; add skeleton lib dir
    (push skel-lib additional-sources)
    ;;
    (when additional-sources
      (dolist (src additional-sources)
        (let ((paths (directory
                      (merge-pathnames
                       (directory-wildcard
                        (if (stringp src) (parse-namestring src) (eval src)))))))
          (dolist (p paths)
            (when (directory-pathname-p p)
              (push p asdf:*central-registry*))))))))

(defun define-dummy-project (name)
  (check-type name symbol)
  (let* ((system (asdf:find-system name nil))
         (project
          (when system
            (make-instance 'project
                           :name name
                           :description (try (slot-value system 'asdf/system::description))))))
    (jlog:dbg "Defining dummy project after incompatible system ``~a''" name)
    (if project
        (setf (gethash name jabs::*jabs-project-registry*) project)
        (jlog:crit "Can not define dummy project ``~a''. Parent system not found" name))
    project))

;; TODO: make wrapper
(add-hook *define-project-hook* #'define-asdf-system)
(add-hook *define-project-hook* #'set-additional-sources)

;;;; Redefine defsystem to define JABS project
(in-package :asdf)

(locally
    (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
  (handler-bind
      (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))

    (defmacro defsystem (name &body options)
      ;; FIXME: very-very-very dirty hack to load systems, like ...:class require-system
      (if (or (member :class options)
              (member :default-component-class options))
          ;; FIXME: skip project creation of systems with ``:class'' inclusion
          `(progn
             (jlog:dbg "Special ASDF system ``~a''. Registering only system" ',name)
             (apply 'register-system-definition ',name ',options)
	     (asdf@core@plugin@jabs::define-dummy-project ,(tools@jabs:tokeyword name)))
          `(jabs::register-project
            ,(tools@jabs:tokeyword name)
            ',(append
               '(:skeleton :flat) ; register ASDF system as project with flat skeleton
               options))))))

;; load some files for backward compatability with old ASDF versions
(load (merge-pathnames
       (make-pathname :name "backward-interface" :type "lisp")
       *load-truename*))

(load (merge-pathnames
       (make-pathname :name "backward-internals" :type "lisp")
       *load-truename*))

;;;; Some macros for better ASDF compatability
(in-package :jabs)

(defmacro version< (version1 version2)
  "Given two version strings, return T if the second is strictly newer"
  `(asdf::version< ,version1 ,version2))

(defmacro version<= (version1 version2)
  "Given two version strings, return T if the second is newer or the same"
  `(asdf::version<= ,version1 ,version2))

(defmacro asdf-version ()
  `(asdf::asdf-version))

(use-package :uiop/package)

(use-package :asdf/parse-defsystem)

(export '(version< version<= asdf-version))
