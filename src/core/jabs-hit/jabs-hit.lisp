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
(defpackage hit@core@plugin@jabs
  (:use :cl :tools@jabs :jabs :jre))

(in-package :hit@core@plugin@jabs)

(make-instance 'jabs::plugin :name :hit :type :core :version jabs::+jabs-version+)

(in-package :jabs)
(export
 '(find-hit
   run-hit
   defhit))

(defvar *jabs-hit-directories*
  (list
   (merge-pathnames (make-pathname :directory '(:relative "hits")) *jabs-share-directory*)
   (merge-pathnames (make-pathname :directory '(:relative ".jabs" "hits")) (user-homedir-pathname))
   (merge-pathnames (make-pathname :directory '(:relative ".jabs" "hits")) (os-pwd))))

(defvar *jabs-hit-registry* (make-hash-table))
(defvar *jabs-hit-template-type* "hit")
(defvar *jabs-hits-to-run* nil)

(defun find-hit-file (name)
  (check-type name (or string symbol))
  (let ((hit-name (string-downcase (princ-to-string name)))
        (hit-dirs))
    (dolist (dirs *jabs-hit-directories*)
      (dolist (dir (directory
                    (merge-pathnames
                     (make-pathname :name hit-name :type *jabs-hit-template-type*)
                     dirs)))
        (push dir hit-dirs)))
    (car hit-dirs)))

(defun parse-hit-from-file (file)
  (check-type file (or string pathname))
  (jlog:dbg "Processing hit file ``~a''" file)
  (let* ((file-path (if (typep file 'string) (parse-namestring file) file))
         (exp (car (os-cat file :list)))
         (name (tosymbol (car exp)))
         (deps (get-option-suffix :depends-on exp))
         (body (cdr (remove-with-arg :depends-on exp))))
    (if (and (not (null name)) (eq name (tosymbol (pathname-name file-path))))
        (eval (append (list 'defhit name deps) body))
        (jlog:crit "Incorrect hit ``~a'' format from file ``~a''" name file))))

(defun find-hit (name)
  "Find hit in registry"
  (check-type name keyword)
  (or
   (gethash name *jabs-hit-registry*)
   (let ((hit-file (find-hit-file name)))
     (when hit-file
       (parse-hit-from-file hit-file)
       (gethash name *jabs-hit-registry*)))))

(defmacro defhit (name depends-on &body body)
  `(register-hit ,(tosymbol name) ',depends-on '(progn ,@body)))

(defun register-hit (name depends-on body)
  (check-type name keyword)
  (check-type depends-on list)
  (check-type body list)
  ;;
  (jlog:dbg "Registering hit ``~a''" name)
  (setf (gethash name *jabs-hit-registry*)
        (list depends-on (eval (append '(lambda nil) (list body))))))

(defun check-hit-dependencies (name &key parent-hits)
  (check-type parent-hits list)
  ;;
  (let ((hit (find-hit name)))
    (if (or (null hit) (null (car hit))) t
        (dolist (dep (car hit))
          (if (member name parent-hits)
              (jlog:crit "Cycle hit dependencies detected in hit ``~a'': ``~a''" name (cons name parent-hits))
              (check-hit-dependencies dep :parent-hits (cons name parent-hits)))))))

(defun run-hit (name &key nodeps)       ; nodeps - ignore all dependencies
  (let ((project (get-project-name *jabs-current-project*)))
    (when (not nodeps)
      (progn
        (jlog:info "Checking hit ``~a'' dependencies in project ``~a''" name project)
        (check-hit-dependencies name)))
    ;;
    (let ((hit (find-hit name)))
      (cond ((not (null hit))
             (jlog:note "...[ Launching hit ``~a'' for project ``~a'' ]" name project)
             (when (and (not (null (car hit))) (null nodeps))
               (dolist (dep (car hit))
                 (jlog:dbg "Launching hit ``~a'' as dependency for ``~a'' in project ``~a''" dep name project)
                 (run-hit (tokeyword dep))))
             (if (not (typep (cadr hit) 'function))
                 (jlog:crit "Incorrect format of hit ``~a'' in project ``~a''" name project)
                 (progn
                   (jlog:dbg "Running hit ``~a'' for project ``~a''" name project)
                   (funcall (cadr hit))
                   (jlog:note "...[ DONE hit ``~a'' for project ``~a'' ]" name project)
                   t)))
            (t (jlog:info "Hit ``~a'' in project ``~a'' is empty. Nothing to do" name project))))))
;; (declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))

;; (bind-jabs-cli-parameter
;;  "hits"
;;  #'(lambda (&rest x)
;;      (dolist (hit x)
;;        (push (string-upcase (princ-to-string hit)) *jabs-hits-to-run*))))
