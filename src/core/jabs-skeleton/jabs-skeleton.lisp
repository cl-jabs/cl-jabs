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
(defpackage skeleton@core@plugin@jabs
  (:use :cl :tools@jabs :re@jabs :jabs))

(in-package :skeleton@core@plugin@jabs)

(make-instance 'jabs::plugin :name :skeleton :type :core :version jabs::+jabs-version+)

(in-package :jabs)

(export '(defskeleton
          find-skeleton
          ;;           run-skeleton
          skeleton
          ;;
          get-skeleton-name
          get-skeleton-src
          get-skeleton-bin
          get-skeleton-lib
          get-skeleton-doc
          get-skeleton-share
          get-skeleton-test
          get-skeleton-contrib
          get-skeleton-conf
          get-skeleton-public
          get-skeleton-script
          get-skeleton-cache
          get-skeleton-log
          get-skeleton-template
          get-skeleton-opt
          get-skeleton-tmp
          get-skeleton-target
          get-skeleton-readme-file
          get-skeleton-license-file
          get-skeleton-install-file
          ;;
          *jabs-default-skeleton-name*
          load-skeleton
          ))

(defvar *jabs-skeleton-directories*
  (list
   (merge-pathnames (make-pathname :directory '(:relative "skeletons")) *jabs-share-directory*)
   (merge-pathnames (make-pathname :directory '(:relative ".jabs" "skeletons")) (user-homedir-pathname))
   (merge-pathnames (make-pathname :directory '(:relative ".jabs" "skeletons")) (os-pwd))
   ))

(defvar *jabs-default-skeleton-name* :default)
(defvar *jabs-skeleton-registry* (make-hash-table))
(defvar *jabs-current-skeleton* nil)
(defvar *jabs-skeleton-template-type* "skl")

(defclass skeleton ()
  ((name          :accessor get-skeleton-name          :initarg :name)
   (src           :accessor get-skeleton-src           :initarg :src)
   (bin           :accessor get-skeleton-bin           :initarg :bin)
   (lib           :accessor get-skeleton-lib           :initarg :lib)
   (doc           :accessor get-skeleton-doc           :initarg :doc)
   (share         :accessor get-skeleton-share         :initarg :share)
   (test          :accessor get-skeleton-test          :initarg :test)
   (contrib       :accessor get-skeleton-contrib       :initarg :contrib)
   (conf          :accessor get-skeleton-conf          :initarg :conf)
   (public        :accessor get-skeleton-public        :initarg :public)
   (script        :accessor get-skeleton-script        :initarg :script)
   (cache         :accessor get-skeleton-cache         :initarg :cache)
   (log           :accessor get-skeleton-log           :initarg :log)
   (template      :accessor get-skeleton-template      :initarg :template)
   ;; other optional directories, not /opt :)
   (opt           :accessor get-skeleton-opt           :initarg :opt)
   (tmp           :accessor get-skeleton-tmp           :initarg :tmp)
   (target        :accessor get-skeleton-target        :initarg :target)
   (readme-file   :accessor get-skeleton-readme-file   :initarg :readme-file)
   (license-file  :accessor get-skeleton-license-file  :initarg :license-file)
   (install-file  :accessor get-skeleton-install-file  :initarg :install-file)))

;;;; functions
(defun skeletondir-p (dir)
  "Skeleton directory record must be a string, or list
   in format

 (\"dirname\" 755 :required)
or
 (\"dirname\" :optional)
or
 (\"dirname\" 511)
or
 \"dirname\"

It`s optional by default
"
  (flet ((ro (symbol)
           "check for :required or :optional"
           (when (or (eq :required symbol) (eq :optional symbol)) t)))
	  (when (or (null dir)
              (stringp dir)
              (pathnamep dir)
              (and (listp dir)
                   ;; 1st arg
                   (or (stringp (car dir))
                       (pathnamep (car dir)))
                   ;; 2nd arg
                   (or (integerp (nth 1 dir))
                       (ro (nth 1 dir)))
                   ;; 3rd arg
                   (or (null (nth 2 dir))
                       (ro (nth 2 dir)))
                   (null (nth 3 dir))))
	    t)))

(defun listskeletondirs-p (dirs)
  "Skeleton can take list of optional directories, which are
not included to main schema (like socket or dev dir etc)"
  (cond ((null dirs) t)
        ((atom dirs) nil)
        (t (when (skeletondir-p (car dirs))
             (listskeletondirs-p (cdr dirs))))))

(deftype skeletondir ()
  '(satisfies skeletondir-p))

(deftype listskeletondirs ()
  '(satisfies listskeletondirs-p))

(defun register-skeleton (name &key bin lib src test conf share doc
                                  contrib public script cache log template opt tmp
                                  target readme-file license-file install-file)
  (check-type name keyword)
  (check-type bin (or null skeletondir))
  (check-type lib (or null skeletondir))
  (check-type src (or null skeletondir))
  (check-type test (or null skeletondir))
  (check-type conf (or null skeletondir))
  (check-type share (or null skeletondir))
  (check-type doc (or null skeletondir))
  (check-type contrib (or null skeletondir))
  (check-type public (or null skeletondir))
  (check-type script (or null skeletondir))
  (check-type cache (or null skeletondir))
  (check-type log (or null skeletondir))
  (check-type template (or null skeletondir))
  (check-type tmp (or null skeletondir))
  (check-type opt (or null listskeletondirs))
  (check-type target (or null skeletondir))
  (check-type readme-file (or null string))
  (check-type license-file (or null string))
  (check-type install-file (or null string))
  (jlog:dbg "Registering skeleton ``~a''" name)
  (setf
   (gethash name *jabs-skeleton-registry*)
   (make-instance
    'skeleton
    :name name
    :bin bin :lib lib
    :src src :test test :conf conf
    :share share :doc doc
    :contrib contrib :public public
    :script script :cache cache :log log
    :template template :tmp tmp :opt opt
    :target target
    :readme-file readme-file
    :license-file license-file
    :install-file install-file))
  (jlog:info "Skeleton ``~a'' registered" name))

(defmacro defskeleton (name &body options)
  `(apply 'register-skeleton ,(tosymbol name) ',options))

(defgeneric initialize-license (skeleton)
  )

(defmethod initialize-license ((skeleton skeleton)) ;; TODO:
  t)

(defgeneric initialize-readme (skeleton)
  )

(defmethod initialize-readme ((skeleton skeleton)) ;; TODO:
  t)

(defgeneric initialize-install (skeleton)
  )

(defmethod initialize-install ((skeleton skeleton)) ;; TODO:
  t)

(defun make-skeleton-directory (dir &key prefix force) ;; FIXME: rewrite for correct pathnames
  (check-type dir skeletondir)
  (check-type prefix (or null string pathname))
  (when (or (and
             (listp dir)
             (or (eq (nth 1 dir) :required)
                 (eq (nth 2 dir) :required)))
            (not (null force)))
    (let* ((path-dir (pathname-as-directory (parse-namestring (or (try (car dir)) dir))))
           (path-prefix (if (stringp prefix) (pathname-as-directory (parse-namestring prefix)) prefix))
           (mode (when (and (listp dir) (integerp (nth 1 dir))) (nth 1 dir)))
           list
           (full-dir-path (merge-pathnames path-dir path-prefix)))
      ;;
      (push full-dir-path list)
      ;; add mode
      (when (not (null mode))
        (nconc list (list :mode mode)))
      (cond ((and
              (file-exists-p full-dir-path)
              (not (directory-pathname-p full-dir-path)))
             (jlog:err "Directory ``~a'' is regular file. Can not create directory" full-dir-path))
            ((directory-exists-p (merge-pathnames path-dir path-prefix))
             (jlog:dbg "Directory ``~a'' already exists. Skipping" full-dir-path))
            (t
             (jlog:info "Making directory ``~a''" full-dir-path)
             (apply 'ensure-directories-exist list))))))

(defgeneric initialize-skeleton (skeleton &key prefix force)
  )

(defmethod initialize-skeleton ((skeleton skeleton) &key (prefix "") (force nil))
  ;; directories-init
  (let ((dirs (list
               (get-skeleton-bin skeleton)
               (get-skeleton-lib skeleton)
               (get-skeleton-src skeleton)
               (get-skeleton-test skeleton)
               (get-skeleton-conf skeleton)
               (get-skeleton-share skeleton)
               (get-skeleton-doc skeleton)
               (get-skeleton-contrib skeleton)
               (get-skeleton-public skeleton)
               (get-skeleton-script skeleton)
               (get-skeleton-cache skeleton)
               (get-skeleton-log skeleton)
               (get-skeleton-template skeleton)
               (get-skeleton-tmp skeleton))))
    (jlog:dbg "Initializing skeleton ``~a'' in ``~a''"
              (get-skeleton-name skeleton)
              (merge-pathnames (pathname-as-directory (if (stringp prefix) (parse-namestring prefix) prefix))
                               (pathname-as-directory (os-pwd))))
    (dolist (dir (remove nil dirs))
      (make-skeleton-directory dir :prefix prefix :force force)))
  ;;
  (when (not (null (get-skeleton-opt skeleton)))
    (dolist (dir (get-skeleton-opt skeleton))
      (make-skeleton-directory dir :prefix prefix :force force)))
  ;; initializing standard files
  ;; (initialize-license skeleton)
  ;; (initialize-readme skeleton)
  ;; (initialize-install skeleton)
  (jlog:info ".[ Initialized skeleton ``~a'' in ``~a'' ]"
             (get-skeleton-name skeleton)
             (merge-pathnames (pathname-as-directory (if (stringp prefix) (parse-namestring prefix) prefix))
                              (pathname-as-directory (os-pwd))))
  )

;; (defskeleton default
;;   :bin "bin1"
;;   :lib ("lib1")
;;   :src ((:relative "src1") 0770 :required))

;; (initialize-skeleton (find-skeleton :test))

(defun find-skeleton-file (name)
  (check-type name (or string symbol))
  (let ((skeleton-name (string-downcase (princ-to-string name)))
        (skeleton-dirs))
    (dolist (dirs *jabs-skeleton-directories*)
      (dolist (dir (directory
                    (merge-pathnames
                     (make-pathname :name skeleton-name :type *jabs-skeleton-template-type*)
                     dirs)))
        (push dir skeleton-dirs)))
    (car skeleton-dirs)))

(defun parse-skeleton-from-file (file)
  (check-type file (or string pathname))
  (let* ((content (car (os-cat file :list)))
         (skeleton-name (tosymbol (car content)))
         (skeleton-body (cdr content)))
    ;;
    (jlog:info "Processing skeleton file ``~a''" file)
    (eval (cons 'defskeleton (cons skeleton-name skeleton-body)))))

(defun find-skeleton (name)
  (check-type name keyword)
  (let ((maybe-skeleton
         (gethash name *jabs-skeleton-registry*)))
    (cond ((null maybe-skeleton)
           (jlog:dbg "Skeleton ``~a'' not found in registry. Trying to load from file"
                     name)
           ;;
           (let ((maybe-skeleton-file (find-skeleton-file name)))
             (if maybe-skeleton-file
                 (progn
                   (parse-skeleton-from-file maybe-skeleton-file)
                   (gethash name *jabs-skeleton-registry*))
                 (jlog:err "Can not find skeleton ``~a''" name))))
          (t maybe-skeleton))))

(defun process-skeleton (skeleton-params)
  "process skeleton"
  (let* ((project-name (get-project-name *jabs-current-project*))
         (skeleton-name (if (atom skeleton-params) skeleton-params (car skeleton-params)))
         (skeleton-force (when (listp skeleton-params) (cadr skeleton-params)))
         (skeleton-obj (if skeleton-name
                            (find-skeleton skeleton-name)
                            (progn
                              (jlog:wrn "There is no skeleton ``~a'' for project ``~a''. Using default: ``~a''"
                                        skeleton-name project-name *jabs-default-skeleton-name*)
                              (find-skeleton *jabs-default-skeleton-name*)))))
    ;;
    (when (null skeleton-obj)
      (jlog:crit "There is no skeleton for project ``~a'' or default skeelethon ``~a''. Check your installation consistency"
                 project-name *jabs-default-skeleton-name*))
    (if skeleton-force
        (jlog:dbg "Processing skeleton ``~a'' for project ``~a'' FORCE" skeleton-name project-name)
        (jlog:dbg "Processing skeleton ``~a'' for project ``~a''" skeleton-name project-name))
    (setf skeleton-name (tosymbol (gensym "SKELETON-")))
    (initialize-skeleton skeleton-obj :force skeleton-force)))

(defun load-skeleton (skeleton)
  (let ((skeleton-name (if (listp skeleton) (car skeleton) skeleton)))
    (when (and (not (find-skeleton skeleton-name)) (or (atom skeleton) (null (cdr skeleton)) (eq (cdr skeleton) :force)))
      (let ((file-to-load
	     (find-skeleton-file (string-downcase (princ-to-string skeleton-name)))))
	(if file-to-load
	    (parse-skeleton-from-file file-to-load)
	  (jlog:crit "There is no file for skeleton ``~a''" skeleton-name))))))

;;;;;;;;;;

(defgeneric get-project-skeleton-name (project)
  )

(defmethod get-project-skeleton-name ((project project))
  (let ((maybe-skeleton-name (try (slot-value project 'skeleton))))
    (cond ((null maybe-skeleton-name)
           (jlog:dbg "No skeleton defined for project ``~a''. Falling back to default name"
                     (get-project-name project))
           (tokeyword *jabs-default-skeleton-name*))
          ((listp maybe-skeleton-name)
           (tokeyword (car maybe-skeleton-name)))
          (t (tokeyword maybe-skeleton-name)))))

(defgeneric project-skeleton-force-p (project)
  )

(defmethod project-skeleton-force-p ((project project))
  "Check if project skeleton required to be initialized with all directories"
  (try (member :force (slot-value project 'skeleton))))

(defgeneric find-project-skeleton (project)
  )

(defmethod find-project-skeleton ((project project))
  "Try to find project skeleton. Use default, if not defined"
  (find-skeleton (get-project-skeleton-name project)))

(defgeneric load-project-skeleton (project)
  )

(defmethod load-project-skeleton ((project project))
  (let ((skeleton
         (or
          (try (slot-value project 'skeleton))
          (list *jabs-default-skeleton-name*))))
    (load-skeleton skeleton)))

(defgeneric process-project-skeleton (project)
  )

(defmethod process-project-skeleton ((project project))
  (let ((skeleton (or
                   (try (slot-value project 'skeleton))
                   (list *jabs-default-skeleton-name*))))
    (process-skeleton skeleton)))
;;;;;;;;;;

(defmacro make-get-project-smth-dir (dir)
  `(progn
     (defgeneric ,(concat-symbols :jabs "project-" dir "-dir") (project))
     (defmethod ,(concat-symbols :jabs "project-" dir "-dir") ((project project))
       (let ((dir (parse-namestring
                   (,(concat-symbols :jabs "get-skeleton-" dir)
                     (find-skeleton (get-project-skeleton-name project)))))
             (project-pathname (get-project-pathname project))
             (workdir (os-pwd)))
         (cond ((eq :absolute (car (pathname-directory dir))) dir)
               ((eq :absolute (car (pathname-directory project-pathname)))
                (merge-pathnames dir project-pathname))
               (t (merge-n-directories workdir project-pathname dir)))))))

(dolist (d (list
            "src"
            "bin"
            "lib"
            "doc"
            "share"
            "test"
            "contrib"
            "conf"
            "public"
            "script"
            "cache"
            "log"
            "template"
            "opt"
            "tmp"
            "target"
            ))
  (eval `(make-get-project-smth-dir ,d)))

;; TODO: skeleton:create-from-project
;; TODO: show list of skeletons

;; TODO: +add project name to skeleton dirs
;; TODO: +add package naming convension
;; TODO: +add build.jab sample for all skeletons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; extending defproject syntax
(bind-project-symbol
 :skeleton
 #'(lambda (x)
     (declare (ignore x))))

;;;; adding define-project-hook to load skeleton
(add-hook *define-project-hook* #'load-project-skeleton)

;;;; adding run-project-hook to process skeleton
;; (add-hook *run-project-hook* #'process-project-skeleton)
