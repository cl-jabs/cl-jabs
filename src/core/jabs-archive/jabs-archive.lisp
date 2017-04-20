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
(defpackage archive@core@plugin@jabs
  (:use :cl :tools@jabs :re@jabs :jabs
	:skeleton@core@plugin@jabs))

(in-package :archive@core@plugin@jabs)

(make-instance 'jabs::plugin :name :archive :type :core :version jabs::+jabs-version+)



;; (defclass skeleton ()
;;   ((name          :accessor get-skeleton-name          :initarg :name)
;;    (src           :accessor get-skeleton-src           :initarg :src)
;;    (bin           :accessor get-skeleton-bin           :initarg :bin)
;;    (lib           :accessor get-skeleton-lib           :initarg :lib)
;;    (doc           :accessor get-skeleton-doc           :initarg :doc)
;;    (share         :accessor get-skeleton-share         :initarg :share)
;;    (test          :accessor get-skeleton-test          :initarg :test)
;;    (contrib       :accessor get-skeleton-contrib       :initarg :contrib)
;;    (conf          :accessor get-skeleton-conf          :initarg :conf)
;;    (public        :accessor get-skeleton-public        :initarg :public)
;;    (script        :accessor get-skeleton-script        :initarg :script)
;;    (cache         :accessor get-skeleton-cache         :initarg :cache)
;;    (log           :accessor get-skeleton-log           :initarg :log)
;;    (template      :accessor get-skeleton-template      :initarg :template)
;;    ;; other optional directories, not /opt :)
;;    (opt           :accessor get-skeleton-opt           :initarg :opt)
;;    (tmp           :accessor get-skeleton-tmp           :initarg :tmp)
;;    (target        :accessor get-skeleton-target        :initarg :target)
;;    (readme-file   :accessor get-skeleton-readme-file   :initarg :readme-file)
;;    (license-file  :accessor get-skeleton-license-file  :initarg :license-file)
;;    (install-file  :accessor get-skeleton-install-file  :initarg :install-file)))

(defvar *archive-files-directories* '(:bin :src :conf :share :public :script))
(defvar *archive-extended-files-directories* '(:contrib :opt))
(defvar *archive-doc-files-directories* '(:doc))
(defvar *archive-readme-file* :readme-file)
(defvar *archive-license-file* :license-file)
(defvar *archive-install-file* :install-file)

(defvar *archive-pre-script* nil)
(defvar *archive-post-script* nil)

(define-plugin-type :archive plugin
  (let ((checker t)
        (plugin-archive-name
         (eval (append '(concat-keywords-w-delimiter *jabs-universal-delimiter*)
                       (list (get-plugin-name plugin))
                       (list (get-plugin-type plugin))
                       jabs::+jabs-plugin-namespace+))))
    ;; TODO: check for functions format (args and their types)
    (dolist (v '(:make-project-archive))
      (when (not (try (symbol-function (tosymbol v plugin-archive-name))))
        (setf checker nil)))
    checker))

;; (bind-project-symbol
;;  :archive
;;  #'(lambda (x)
;;      (check-type x list)
;;      (let ((name (cadr (member :name x)))
;; 	   (types (cadr (member :types x)))
;; 	   (version (cadr (member :version x))))
;;        (format nil "~a ~a ~a" name types version))))

(defgeneric make-project-archive (project)
  )

(defmethod make-project-archives ((project project))
  "Make project archive(s) from target directory"
  (let ((name (project-slot-value project 'jabs::name))
        (version (project-slot-value project 'jabs::version))
        (target-dir (get-skeleton-target (find-project-skeleton project)))
        (plugins (filter-project-plugins-by-type project :archive)))


    (plugins-api-call-all :make-project-archive plugins name version target-dir)))

;; (defmethod pack-project ((project project))
;;   "Check if project can be reachable in all accessible plugins"
;;   (let ((name (project-slot-value project 'jabs::name))
        ;;         (version (project-slot-value project 'jabs::version))
;;         (source (get-skeleton-target (find-project-skeleton project)))
;;         (plugins (filter-project-plugins-by-type project :repository)))
;;     (plugins-api-call-to-true
;;      :make-archive plugins name version source)))

;; TODO:
;; pack-project
;; unpack-project


;; (defproject zzz
;;     :name "ZZZ"
;;     :plugins (:tgz@archive :deb@archive)
;;     :archive (:name "test" :types (:deb :rpm) :version "0.1.2")
