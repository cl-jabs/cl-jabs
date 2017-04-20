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
;; (defpackage project@core@plugin@jabs
;;   (:use :cl :tools@jabs :re@jabs :jabs))

;; (in-package :project@core@plugin@jabs)

;; (make-instance 'jabs::plugin :name :project :type :core :version jabs::+jabs-version+)

(in-package :jabs)

(export '(find-project
          run-project
          bind-project-symbol
          project
          get-project-name
          get-project-description
          get-project-pathname
          ;;
          *define-project-hook*
          *jabs-current-project*
          ;;
          defproject
          *jabs-project-to-run*
          project-slot-value
          ))

;;;; variables
;; Projects
(defvar *jabs-project-registry* (make-hash-table))
(defvar *jabs-project-to-run* nil) ;; when null, run all projects in registry
(defvar *jabs-current-project* nil)
(defvar *jabs-add-project* nil)

;; Hooks
(defvar *define-project-hook* nil
  "Run some functions when project defining with project as argument after bound symbols processing")

(defvar *pre-run-project-hook* nil
  "Run some functions without arguments before project will be launched")

(defvar *run-project-hook* nil
  "Run some functions with project as argument after bound symbols processing")

(defvar *post-run-project-hook* nil
  "Run some functions without arguments after project launched")

;;;; bind CLI param to set project force
(bind-jabs-cli-parameter
 "project"
 #'(lambda (&rest x)
     (dolist (proj x)
       (setf *jabs-project-to-run* (tokeyword proj)))))

(process-jabs-cli-parameter "project")

;;;;
;;;; load and process projects
;;;;

(defclass project ()
  ((name        :accessor get-project-name        :initarg :name)
   (description :accessor get-project-description :initarg :description :initform "")
   (pathname    :accessor get-project-pathname    :initarg :pathname :initform "")))

;; do not inherit project class. TODO: is it needed?
#+sbcl(sb-mop:finalize-inheritance (find-class 'project))

(defun find-project (name)
  "Find project"
  (check-type name keyword)
  (gethash name *jabs-project-registry*))

(defvar *jabs-project-bound-symbols* (make-hash-table)
  "Define symbols, bound to project as recognizable (no syntax error)")

(defun bind-project-symbol (project-symbol function)
  "Bind project symbol to some function"
  (check-type project-symbol keyword)
  (check-type function function)
  (when (gethash project-symbol *jabs-project-bound-symbols*)
    (jlog:wrn "Project symbol ``~a'' already bound to function ``~a''. Overriding"
              project-symbol function))
  (setf (gethash project-symbol *jabs-project-bound-symbols*) function))

(defun register-project (name args)
  "Register project to *jabs-project-registry*"
  (check-type name keyword)
  (check-type args list)
  (let ((parent-slots
         #+sbcl(mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (find-class 'project)))
         #-sbcl(jlog:crit "Parent slots in register-project is not implemented!")
         )
        (binary-list) (other-slots) (project) (other-instance-slots))
    (labels ((make-binary-lists (list &optional binary-list)
               (if (null list) binary-list
                   (make-binary-lists (cddr list)
                                      (cons (list (car list) (cadr list)) binary-list)))))
      (jlog:dbg "Creating project ``~a''" name) ;; TODO: make as separate function
	    (setf binary-list (make-binary-lists args))
	    (dolist (v binary-list)
	      (when (not (member (tosymbol (car v)) parent-slots))
          (push (list
                 (tosymbol (car v) :jabs)
                 :initarg
                 (tosymbol (car v)))
                other-slots))
	      (if (atom (cadr v))
            (push (cadr v) other-instance-slots)
            (push `(quote ,(cadr v)) other-instance-slots))
	      (push
         ;; (intern (symbol-name (car v)) (find-package :keyword))
         (tosymbol (car v))
         other-instance-slots))
	    ;;#+sbcl
	    (eval `(defclass ,name (project)
               ,other-slots))
	    ;; initialize ``pathname'' slot
	    (when (not (member :pathname other-instance-slots))
        ;; TODO: realize via smth like 'destructive append'
        (push "" other-instance-slots)
        (push :pathname other-instance-slots))
	    ;;
	    (setf project (eval
                     `(make-instance ',name :name ',name ,@other-instance-slots)))
	    ;; registering project
	    (jlog:dbg "Registering project ``~a''" name)
      (when (gethash name *jabs-project-registry*)
        (jlog:wrn "Project ``~a'' already registered. Overriding" name))
      (setf (gethash name *jabs-project-registry*) project)
      (let ((*jabs-current-project* project))
	(run-hook *define-project-hook* project))
      ;; return
      (jlog:info "Project ``~a'' registered" name)
      t
      )))

(defmacro defproject (name &body options)
  "Register project to jabs-project-registry"
  `(progn
	  (when *jabs-add-project*
	    (setf *jabs-project-to-run* ,(tokeyword name))
	    (setf *jabs-add-project* nil))
	  (register-project ,(tosymbol name) ',options)))

(defgeneric run-project (project)
  )

(defmethod run-project ((project project))
  "Run project"
  (jlog:note "[ Launching project ``~a'' ]" (get-project-name project))
  (let ((*jabs-current-project* project))
    ;; pre-run hook
    (let ((*jabs-current-project* project))
      (run-hook *pre-run-project-hook* project))
    ;;
    (let ((slots
           #+sbcl(mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of project)))
           #-sbcl(jlog:crit "Slots not implemented in run-project!")
           ))
      ;; run project
      (dolist (v slots)
        (let* ((slot-symbol (tosymbol v))
               (bound-symbol (gethash slot-symbol *jabs-project-bound-symbols*))
               (maybe-bound-slot (try (slot-value project v))))
          ;; check if syntax correct
          (when (not bound-symbol)
            (jlog:crit "Incorrect project format. Unknown symbol ``~a''" v))
          ;;
          (funcall bound-symbol maybe-bound-slot))))
    ;; post-run hook
    (let ((*jabs-current-project* project))
      (run-hook *run-project-hook* project))
    (jlog:note "[ DONE project ``~a'' ]" (get-project-name project))))

(defgeneric project-slot-value (project slot)
  )

(defmethod project-slot-value ((project project) slot)
  "Get slot value from project class"
  (try (slot-value project slot)))

(defgeneric set-project-slot-value (project slot value)
  )

(defmethod set-project-slot-value ((project project) slot value)
  (if (not (nth-value 1 (try (slot-value project slot))))
      (setf (slot-value project slot) value)
      (jlog:crit "Can not set parameter ``~a'' for project ``~a''. " slot (get-project-name project))))

(defsetf project-slot-value set-project-slot-value)
