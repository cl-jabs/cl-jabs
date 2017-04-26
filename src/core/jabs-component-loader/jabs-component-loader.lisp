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
(defpackage component-loader@core@plugin@jabs
  (:use :cl :tools@jabs :jabs :jre))

(in-package :component-loader@core@plugin@jabs)

(make-instance 'jabs::plugin :name :component-loader :type :core :version jabs::+jabs-version+)

(in-package :jabs)

(bind-project-symbol :components #'(lambda (&rest x) (declare (ignore x)) t))

;; (export
;;  '(find-component-loader
;;    run-component-loader
;;    defcomponent-loader))

;; (defvar *jabs-component-loader-directories*
;;   (list
;;    (merge-pathnames (make-pathname :directory '(:relative "component-loaders")) *jabs-share-directory*)
;;    (merge-pathnames (make-pathname :directory '(:relative ".jabs" "component-loaders")) (user-homedir-pathname))
;;    (merge-pathnames (make-pathname :directory '(:relative ".jabs" "component-loaders")) +jabs-run-directory+)))

;; (defvar *jabs-component-loader-registry* (make-hash-table))
;; (defvar *jabs-component-loader-template-type* "component-loader")
;; (defvar *jabs-component-loaders-to-run* nil)

;; (defun find-component-loader-file (name)
;;   (check-type name (or string symbol))
;;   (let ((component-loader-name (string-downcase (princ-to-string name)))
;;         (component-loader-dirs))
;;     (dolist (dirs *jabs-component-loader-directories*)
;;       (dolist (dir (directory
;;                     (merge-pathnames
;;                      (make-pathname :name component-loader-name :type *jabs-component-loader-template-type*)
;;                      dirs)))
;;         (push dir component-loader-dirs)))
;;     (car component-loader-dirs)))

;; (defun parse-component-loader-from-file (file)
;;   (check-type file (or string pathname))
;;   (jlog:dbg "Processing component-loader file ``~a''" file)
;;   (let* ((file-path (if (typep file 'string) (parse-namestring file) file))
;;          (exp (car (os-cat file :list)))
;;          (name (tosymbol (car exp)))
;;          (deps (get-option-suffix :depends-on exp))
;;          (body (cdr (remove-with-arg :depends-on exp))))
;;     (if (and (not (null name)) (eq name (tosymbol (pathname-name file-path))))
;;         (eval (append (list 'defcomponent-loader name deps) body))
;;         (jlog:crit "Incorrect component-loader ``~a'' format from file ``~a''" name file))))

;; (defun find-component-loader (name)
;;   "Find component-loader in registry"
;;   (check-type name keyword)
;;   (or
;;    (gethash name *jabs-component-loader-registry*)
;;    (let ((component-loader-file (find-component-loader-file name)))
;;      (when component-loader-file
;;        (parse-component-loader-from-file component-loader-file)
;;        (gethash name *jabs-component-loader-registry*)))))

;; (defmacro defcomponent-loader (name depends-on &body body)
;;   `(register-component-loader ,(tosymbol name) ',depends-on '(progn ,@body)))

;; (defun register-component-loader (name depends-on body)
;;   (check-type name keyword)
;;   (check-type depends-on list)
;;   (check-type body list)
;;   ;;
;;   (jlog:dbg "Registering component-loader ``~a''" name)
;;   (setf (gethash name *jabs-component-loader-registry*)
;;         (list depends-on (eval (append '(lambda nil) (list body))))))

;; (defun check-component-loader-dependencies (name &key parent-component-loaders)
;;   (check-type parent-component-loaders list)
;;   ;;
;;   (let ((component-loader (find-component-loader name)))
;;     (if (or (null component-loader) (null (car component-loader))) t
;;         (dolist (dep (car component-loader))
;;           (if (member name parent-component-loaders)
;;               (jlog:crit "Cycle component-loader dependencies detected in component-loader ``~a'': ``~a''" name (cons name parent-component-loaders))
;;               (check-component-loader-dependencies dep :parent-component-loaders (cons name parent-component-loaders)))))))

;; (defun run-component-loader (name &key nodeps)       ; nodeps - ignore all dependencies
;;   (let ((project (get-project-name *jabs-current-project*)))
;;     (when (not nodeps)
;;       (progn
;;         (jlog:info "Checking component-loader ``~a'' dependencies in project ``~a''" name project)
;;         (check-component-loader-dependencies name)))
;;     ;;
;;     (let ((component-loader (find-component-loader name)))
;;       (cond ((not (null component-loader))
;;              (jlog:note "...[ Launching component-loader ``~a'' for project ``~a'' ]" name project)
;;              (when (and (not (null (car component-loader))) (null nodeps))
;;                (dolist (dep (car component-loader))
;;                  (jlog:dbg "Launching component-loader ``~a'' as dependency for ``~a'' in project ``~a''" dep name project)
;;                  (run-component-loader (tokeyword dep))))
;;              (if (not (typep (cadr component-loader) 'function))
;;                  (jlog:crit "Incorrect format of component-loader ``~a'' in project ``~a''" name project)
;;                  (progn
;;                    (jlog:dbg "Running component-loader ``~a'' for project ``~a''" name project)
;;                    (funcall (cadr component-loader))
;;                    (jlog:note "...[ DONE component-loader ``~a'' for project ``~a'' ]" name project)
;;                    t)))
;;             (t (jlog:info "Component-Loader ``~a'' in project ``~a'' is empty. Nothing to do" name project))))))
;; ;; (declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))

;; ;; (bind-jabs-cli-parameter
;; ;;  "component-loaders"
;; ;;  #'(lambda (&rest x)
;; ;;      (dolist (component-loader x)
;; ;;        (push (string-upcase (princ-to-string component-loader)) *jabs-component-loaders-to-run*))))
