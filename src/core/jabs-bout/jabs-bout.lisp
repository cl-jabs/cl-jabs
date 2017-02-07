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
(defpackage bout@core@plugin@jabs
  (:use :cl :tools@jabs :re@jabs :jabs))

(in-package :bout@core@plugin@jabs)

(make-instance 'jabs::plugin :name :bout :type :core :version jabs::+jabs-version+)

(in-package :jabs)

(export '(defbout))

(defvar *jabs-bout-directories*
  (list
   (merge-pathnames (make-pathname :directory '(:relative "bouts")) *jabs-share-directory*)
   (merge-pathnames (make-pathname :directory '(:relative ".jabs" "bouts")) (user-homedir-pathname))
   (merge-pathnames (make-pathname :directory '(:relative ".jabs" "bouts")) (os-pwd))))

(defvar *jabs-current-bout* nil)
(defvar *jabs-bout-registry* (make-hash-table :test 'equal))
(defvar *jabs-bout-template-type* "bt")
(defvar *jabs-default-bout-name* :default)
(defvar *jabs-bouts-to-run* nil)

(defun register-bout (name &rest rounds)
  "Register bout"
  (check-type name keyword)
  (check-type rounds list)
  (dolist (r rounds)
    (if (not (typep r 'keyword))
        (jlog:crit "Round ``~a'' in bout ``~a'' is not a symbol" r name)
        (progn
          (jlog:dbg "Registering bout ``~a''" name)
          (setf (gethash name *jabs-bout-registry*) rounds)))))

(defun find-bout (name)
  (check-type name keyword)
  (gethash name *jabs-bout-registry*))

(defgeneric run-bout (project name &optional round)
  )

(defmethod run-bout ((project project) name &optional round)
  (check-type name keyword)
  (check-type round (or keyword null))
  (let ((bout (find-bout name))
        (rounds-to-run))
    (if (not bout)
        (jlog:crit "There is no bout, named ``~a''" name)
        (progn
          (jlog:note ".[ Launching bout ``~a'' for project ``~a'' ]" name (get-project-name project))
          (cond ((and round (member round bout))
		 (setf rounds-to-run (reverse (member round (reverse bout)))))
		((and round (not (member round bout)))
		 (jlog:crit "Bout ``~a'' requested to run to round ``~a'' for project ``~a''. But but bout does not contain this round. Exiting"
			    name round (get-project-name project)))
		(t (setf rounds-to-run bout)))
          (dolist (rnd rounds-to-run)
            (run-round rnd))
          (jlog:note ".[ DONE bout ``~a'' for project ``~a'' ]" name (get-project-name project))
          ))))

(defmacro defbout (name &body rounds)
  `(register-bout ,(tosymbol name) ,@rounds))

(defun parse-bout-from-file (file)
  "Read bout file and parse it as bout"
  (check-type file (or string pathname))
  (jlog:dbg "Processing bout file: ``~a''" file)
  (let* ((exp (car (os-cat file :list)))
         (name (car exp))
         (rounds (cdr exp)))
    (apply 'register-bout (cons name rounds))))

(defun find-bout-file (name)
  (check-type name keyword)
  (let ((bout-name (string-downcase (princ-to-string name)))
        (bout-dirs))
    (dolist (dir *jabs-bout-directories*)
      (dolist (file (directory
                     (merge-pathnames
                      (make-pathname :name bout-name :type *jabs-bout-template-type*)
                      dir)))
        (when (not (null file))
          (push file bout-dirs))))
    (car bout-dirs)))

;; bind CLI parameter -Dbout=...
(bind-jabs-cli-parameter
 "bouts"
 #'(lambda (&rest x)
     (dolist (bout (reverse x))
       (when (tosymbol bout)
	 (pushnew (tosymbol bout) *jabs-bouts-to-run*)))))

(process-jabs-cli-parameter "bouts")

;; extend 'defproject' definition with 'bout' parameter
(bind-project-symbol
 :bout
 #'(lambda (x)
     (declare (ignore x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric load-bouts-by-name (project bout-names)
  )

(defmethod load-bouts-by-name ((project project) bout-names)
  (check-type bout-names list)
  (dolist (bout-name (or bout-names (list *jabs-default-bout-name*)))
    (let ((current-bout (find-bout bout-name))
          (current-bout-file (find-bout-file bout-name)))
      (when (and
             (not current-bout)
             (eq
              (tokeyword (get-project-name project))
              *jabs-project-to-run*))
        ;;
        (if current-bout-file
            (parse-bout-from-file current-bout-file)
            (jlog:crit "There is no file for bout ``~a''" bout-name))))))

(defgeneric load-project-bouts (project)
  )

(defmethod load-project-bouts ((project project))
  "Load defined, or set bouts for project"
  (if *jabs-bouts-to-run*
      (load-bouts-by-name project *jabs-bouts-to-run*)
      ;;
      (let ((bout-name (or
                        (try (slot-value project 'bout))
                        *jabs-default-bout-name*)))
        (load-bouts-by-name project (list bout-name)))))

(add-hook *define-project-hook* #'load-project-bouts)

(defun insert-round (roundname boutname)
  (let ((bout (find-bout boutname)))
    (push roundname bout)
    (setf (gethash boutname *jabs-bout-registry*) bout)))

(defun append-round (roundname boutname)
  (let ((bout (reverse (find-bout boutname))))
    (push roundname bout)
    (setf (gethash boutname *jabs-bout-registry*) (reverse bout))))

(defun delete-round (roundname boutname)
  (let ((bout (find-bout boutname)))
    (setf (gethash boutname *jabs-bout-registry*) (remove roundname bout))))

(defgeneric run-project-bouts (project)
  )

(defmethod run-project-bouts ((project project))
  "Run pre-defined bouts for project"
  (labels ((pair-b-r (bouts rounds)
             (cond ((null bouts) nil)
                   ((or (null rounds) (null (car rounds)))
                    (cons (cons (car bouts) nil)
                          (pair-b-r (cdr bouts) (cdr rounds))))
                   (t
                    (cons (cons (car bouts) (car rounds))
                          (pair-b-r (cdr bouts) (cdr rounds)))))))
    (let* ((bouts (or *jabs-bouts-to-run*
                      (list
                       (or
                        (project-slot-value project 'bout)
                        *jabs-default-bout-name*))))
           (rounds *jabs-rounds-to-run*)
           (bout-round-pairs (pair-b-r bouts rounds)))

      (if bouts
          (dolist (br bout-round-pairs)
            (run-bout project (car br) (cdr br)))
          (jlog:crit "You did not define bout for project ``~a'' in any way"
                     (get-project-name project))))))

(add-hook *run-project-hook* #'run-project-bouts)

;; TODO: realise round list view for projects
