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
(defpackage round@core@plugin@jabs
  (:use :cl :tools@jabs :re@jabs :jabs))

(in-package :round@core@plugin@jabs)

(make-instance 'jabs::plugin :name :round :type :core :version jabs::+jabs-version+)

(in-package :jabs)

(export '(find-round
          defround
          run-round))

(defvar *jabs-round-directories*
  (list
   (merge-pathnames (make-pathname :directory '(:relative "rounds")) *jabs-share-directory*)
   (merge-pathnames (make-pathname :directory '(:relative ".jabs" "rounds")) (user-homedir-pathname))
   (merge-pathnames (make-pathname :directory '(:relative ".jabs" "rounds")) (os-pwd))))

(defvar *jabs-round-template-type* "rd")
(defvar *jabs-round-registry* (make-hash-table))
(defvar *jabs-rounds-to-run* nil)

(defun register-round (name &rest hits)
  (check-type name keyword)
  (dolist (h hits)
    (when (not (typep h 'keyword))
      (jlog:crit "Hit name ``~a'' in round ``~a'' is not a keyword" h name)))
  (when (not (null name))
    (progn
      (jlog:dbg "Registering round ``~a''" name)
      (setf (gethash name *jabs-round-registry*) hits))))

(defun find-round-file (name)
  (check-type name keyword)
  (let ((round-name (string-downcase (princ-to-string name)))
        (round-files))
    (jlog:dbg "Searching for file with round ``~a''" name)
    (dolist (dir *jabs-round-directories*)
      (dolist (file (directory
                     (merge-pathnames
                      (make-pathname :name round-name :type *jabs-round-template-type*)
                      dir)))
        (push file round-files)))
    (if (not (null round-files))
        (progn
          (jlog:dbg "Found round file ``~a''" (car round-files))
          (car round-files))
        (jlog:dbg "File not found for round ``~a''" name))))

(defun parse-round-from-file (file)
  (check-type file (or string pathname))
  (jlog:dbg "Processing round file ``~a''" file)
  (let* ((file-path (if (typep file 'string) (parse-namestring file) file))
         (exp (car (os-cat file-path :list)))
         (name (car exp))
         (hits (cdr exp)))
    (apply 'register-round (cons name hits))))

(defun find-round (name)
  (check-type name keyword)
  (or
   (gethash name *jabs-round-registry*)
   (let ((round-file (find-round-file name)))
     (when round-file
       (parse-round-from-file round-file)
       (gethash name *jabs-round-registry*)))))

(defun run-round (name &optional hit)
  (check-type name keyword)
  (check-type hit list)
  (let ((round (find-round name)))
    (labels ((run-round-to-hit
                 (round &optional hit)
               (when (and (not (null round)) (or (null hit) (member hit round)))
                 (run-hit (tosymbol (car round)) :nodeps t)
                 (run-round-to-hit (cdr round)))))
      ;;
	    (if hit
          (jlog:note "..[ Launching round ``~a'' to hit ``~a'' for project ``~a'' ]"
                     name hit (get-project-name *jabs-current-project*))
          (jlog:note "..[ Launching round ``~a'' for project ``~a'' ]"
                     name (get-project-name *jabs-current-project*)))
	    (run-round-to-hit round (tosymbol hit))
      ;;
      (if hit
          (jlog:note "..[ DONE round ``~a'' to hit ``~a'' for project ``~a'' ]"
                     name hit (get-project-name *jabs-current-project*))
          (jlog:note "..[ DONE round ``~a'' for project ``~a'' ]"
                     name (get-project-name *jabs-current-project*)))
      t))) ;; TODO: make correct exit stauts

(defmacro defround (name &body hits)
  `(register-round ,(tosymbol name) ,@hits))

(defun insert-hit (hitname roundname)
  "Insert hit to top of round"
  (check-type hitname keyword)
  (check-type roundname keyword)
  (jlog:dbg "Inserting hit ``~a'' to round ``~a''" hitname roundname)
  (let ((round (find-round roundname)))
    (if round
        (progn
          (push hitname round)
          (setf (gethash roundname *jabs-round-registry*) round))
        (jlog:crit "No round ``~a'' found. Can not insert hit ``~a''" roundname hitname))))

(defun append-hit (hitname roundname)
  "Append hit to bottom of round"
  (check-type hitname keyword)
  (check-type roundname keyword)
  (jlog:dbg "Appending hit ``~a'' to round ``~a''" hitname roundname)
  (let ((round (reverse (find-round roundname))))
    (if round
        (progn
          (push hitname round)
          (setf (gethash roundname *jabs-round-registry*) (reverse round)))
        (jlog:crit "No round ``~a'' found. Can not append hit ``~a''" roundname hitname))))

(defun delete-hit (hitname roundname)
  "Delete hit from round"
  (check-type hitname keyword)
  (check-type roundname keyword)
  (jlog:dbg "Removing hit ``~a'' to round ``~a''" hitname roundname)
  (let ((round (find-round roundname)))
    (if round
        (setf (gethash roundname *jabs-round-registry*) (remove hitname round))
        (jlog:crit "No round ``~a'' found. Can not delete hit ``~a''" roundname hitname))))

(bind-jabs-cli-parameter
 "rounds"
 #'(lambda (&rest x)
     (dolist (bout (reverse x))
       (pushnew (tosymbol bout) *jabs-rounds-to-run*))))

(process-jabs-cli-parameter "rounds")
;; TODO: realise hit list view for projects
