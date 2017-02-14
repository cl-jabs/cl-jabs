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

(load (make-pathname :directory '(:relative "test" "unit") :name "header.lisp" :type "in"))

;; (load (make-pathname :directory '(:relative "src") :name "jabs-re" :type "lisp"))
(load (make-pathname :directory '(:relative "src") :name "jabs-tools" :type "lisp"))


(defpackage tools-test
  (:use :cl :test-engine :clunit :tools@jabs))

(in-package :tools-test)
(use-package :clunit)

(defun hash-table-test-name (name)
  ;; Workaround for Clisp calling EQL in a hash-table FASTHASH-EQL.
  (hash-table-test (make-hash-table :test name)))

(declaim (notinline opaque))
(defun opaque (x)
  x)

;; tests
(defsuite tools-suite ())

;; :argv
;; :terminate

;; (defun setup-stdin ()
;; (defvar *stdout* *standard-output*
;; (defun setup-stdout ()
;; (defvar *stderr* *error-output*
;; (defun setup-stderr ()

(deftest flatten-test (tools-suite)
  (assert-equal '(1 2 3) (flatten '(1 2 3)))
  (assert-equal '(1 2 3) (flatten '(1 (2) 3)))
  (assert-equal '(1 2 3 4 5 6 3) (flatten '(1 (2 3 (4 5 6)) 3)))
  (assert-condition type-error (flatten "test")))

;; (defun terminate (&optional (status 0))

;; :tolist
(deftest tolist-test (tools-suite)
  (assert-equal '(#\t #\e #\s #\t) (tolist "test"))
  (assert-equal '(1 2 3) (tolist '(1 2 3))))

;; :tostr
(deftest tostr-test (tools-suite)
  (assert-equal "test" (tostr '(#\t #\e #\s #\t)))
  (assert-equal "123" (tostr '(1 2 3))) ; FIXME:
  (assert-equal "TEST" (tostr :test))
  (assert-equal "q"  (tostr #\q))
  )

;; :tosymbol
(deftest tosymbol-test (tools-suite)
  (assert-eq :test (tosymbol '(#\t #\e #\s #\t)))
  (assert-eq :test (tosymbol "test"))
  (assert-eq :123 (tosymbol "123"))
  (assert-eq (intern "TEST" (find-package :cl-user)) (tosymbol "test" :cl-user))
  (assert-eq :q  (tosymbol #\q))
  )

;; :concat-symbols
(deftest concat-symbols-test (tools-suite)
  (assert-false (concat-symbols :keyword '(#\t #\e #\s #\t))) ;; FIXME: We need to return smth like nil or error
  (assert-eq :test (concat-symbols :keyword #\t #\e #\s #\t))
  (assert-eq :test (concat-symbols :keyword "te" "st"))
  (assert-eq (intern "TEST" (find-package :cl-user)) (concat-symbols :cl-user "te" "st"))
  (assert-eq :123 (concat-symbols :keyword 1 2 3))
  (assert-eq :q  (concat-symbols :keyword #\q))
  )

;; :concat-symbols-w-delimiter
(deftest concat-symbols-w-delimiter-test (tools-suite)
  (assert-false (concat-symbols-w-delimiter :keyword '(#\t #\e #\s #\t))) ;; FIXME: We need to return smth like nil or error
  (assert-eq :T@E@S@T (concat-symbols-w-delimiter "@" :keyword #\t #\e #\s #\t))
  (assert-eq :te#st (concat-symbols-w-delimiter "#" :keyword "te" "st"))
  (assert-eq (intern "TE;ST" (find-package :cl-user)) (concat-symbols-w-delimiter ";" :cl-user "te" "st"))
  (assert-condition type-error (concat-symbols-w-delimiter :keyword 1 2 3))
  )

;; (defun argv (&optional argnumber)

(deftest maphash-keys-test (tools-suite)
  (let ((keys nil)
        (table (make-hash-table)))
    (declare (notinline maphash-keys))
    (dotimes (i 10)
      (setf (gethash i table) t))
    (maphash-keys (lambda (k) (push k keys)) table)
    (assert-equal '(0 1 2 3 4 5 6 7 8 9) (reverse keys))))

(deftest maphash-values-test (tools-suite)
  (let ((vals nil)
	(table (make-hash-table)))
    (declare (notinline maphash-values))
    (dotimes (i 10)
      (setf (gethash i table) (- i)))
    (maphash-values (lambda (v) (push v vals)) table)
    (assert-equal '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9) (reverse vals))))

(deftest copy-hash-table-test (tools-suite)
  (let ((table (make-hash-table)))
    (assert-eq (type-of table) (type-of (copy-hash-table table)))
    (assert-condition type-error (copy-hash-table "test"))))

(deftest component-present-p-test (tools-suite)
  (assert-true (component-present-p "test"))
  (assert-true (component-present-p :test))
  (assert-false (component-present-p nil))
  (assert-false (component-present-p :unspecific)))

(deftest directory-pathname-p (tools-suite)
  (assert-true (directory-pathname-p "/test/"))
  (assert-true (directory-pathname-p (make-pathname :directory '(:relative "test"))))
  (assert-false (directory-pathname-p "/test"))
  (assert-equal "/test/" (directory-pathname-p "/test/"))
  (assert-condition type-error (directory-pathname-p :test))
  )

(deftest pathname-as-directory (tools-suite)
  (assert-true (pathname-as-directory "/test/"))
  (assert-true (pathname-as-directory (make-pathname :directory '(:relative "test"))))
  (assert-equal 'pathname (type-of (pathname-as-directory "/test")))
  (assert-false (pathname-name (pathname-as-directory "/test")))
  (assert-equal (pathname-directory (parse-namestring "/test/")) (pathname-directory (pathname-as-directory "/test")))
  (assert-equal (parse-namestring "/test.lisp/") (pathname-as-directory "/test.lisp"))
  (assert-equal (parse-namestring "test.lisp/") (pathname-as-directory (make-pathname :name "test" :type "lisp")))
  (assert-false (pathname-name (pathname-as-directory "/test.lisp")))
  (assert-condition type-error (pathname-as-directory :test))
  )

(deftest pathname-as-file (tools-suite)
  (assert-true (pathname-as-file "/test/"))
  (assert-true (pathname-as-file (make-pathname :name "test")))
  (assert-true (pathname-name (pathname-as-file "/test")))
  ;; (assert-false (pathname-directory (pathname-as-file "/test"))) ; FIXME:
  (assert-equal (parse-namestring "/test.lisp") (pathname-as-file "/test.lisp"))
  (assert-equal (parse-namestring "test.lisp") (pathname-as-file (make-pathname :name "test" :type "lisp")))
  (assert-condition type-error (pathname-as-file :test))
  )

(deftest directory-wildcard (tools-suite)
  (assert-equal (make-pathname :directory '(:absolute "test") :name :wild :type :wild)  (directory-wildcard "/test"))
  (assert-equal (make-pathname :directory '(:absolute "test") :name :wild :type :wild)
                (directory-wildcard (make-pathname :directory '(:absolute "test"))))
  (assert-condition type-error (directory-wildcard :test))
  )
;; (defun clisp-subdirectories-wildcard (wildcard)
;; (defun list-directory (dirname &key (follow-symlinks t))
;; (defun file-exists-p (pathspec)
;; (defun directory-exists-p (pathspec)
;; (defvar *stream-buffer-size* 8192)
;; (defun copy-stream (from to &optional (checkp t))
;; (defun copy-file (from to &key overwrite)
;; (defun delete-directory-and-files (dirname &key (if-does-not-exist :error))

(deftest get-cars-test (tools-suite)
  (assert-equal
   '(1 5)
   (get-cars '((1 2 3 4) (5 6 7 8)))))


(deftest emptyp-test (tools-suite)
  (assert-true (tools@jabs::emptyp '()))
  (assert-true (tools@jabs::emptyp ""))
  (assert-false (tools@jabs::emptyp '(nil)))
  (assert-false (tools@jabs::emptyp :test))
  )

;; (defun emptyp (x)
;; (defun find-package* (package-designator &optional (error t))
;; (defun find-symbol* (name package-designator &optional (error t))
;; (defun symbol-call (package name &rest args)
;; (defun finish-outputs (&rest streams)
;; (defun featurep (x &optional (*features* *features*))
;; (defun first-feature (feature-sets)
;; (defun ccl-fasl-version ()
;; (defun lisp-version-string ()
;; (defun os-hostname ()
;; (defun parse-unix-namestring* (unix-namestring)
;; (defun os-cd (x)
;; (defun os-dirname (pathname)
;; (defun list-files-recursively (dir &optional files)
;; (defun find-by-name (name list)
;; (defun find-by-type (type list)
;; (defun find-directories (list)
;; (defun merge-n-directories (&rest dirs)
;; (defun os-find (path &key name extension (type t))
;; (defun cat-to-string (file)
;; (defun cat-to-list (file &aux result list (rt (copy-readtable)))
;; (defun os-cat (file &optional (output-type :string)) ; types: :string :list
;; (defun os-pwd ()
;; (defun os-mkdir (dirname)
;; (defun os-cp (from to &key recursive force)
;; (defun os-rm (pathname &key recursive)
;; (defun os-touch (pathname)
;; (defun os-getuid ()
;; (defun os-getenv (x)
;;   (defun os-getenvp (x)
;; (defun implementation-type ()
;; (defvar *implementation-type* (implementation-type)
;; (defun os-architecture ()
;; (defun operating-system ()
;; (defun implementation-identifier ()

;; (defun subtree (name list)
;; (defun replace-subtree (name list sublist)
;; (defun add-subtree (name list sublist)
;; (defun find-file-from-list-by-filename (name files)

(deftest pathname-root-test (tools-suite)
  (assert-equal (parse-namestring "/") (tools@jabs::pathname-root "/home/cosmonaut"))
  (assert-equal (parse-namestring "/") (tools@jabs::pathname-root "./cosmonaut"))
  (assert-condition type-error (tools@jabs::pathname-root :test))
  )
;; (defun pathname-root (pathname)

(deftest absolute-pathname-p-test (tools-suite)
  (assert-true (tools@jabs::absolute-pathname-p (parse-namestring "/home/test")))
  (assert-false (tools@jabs::absolute-pathname-p (parse-namestring "home/test")))
  (assert-false (tools@jabs::absolute-pathname-p :test))
  )
;; (defun truenamize (pathname &optional (defaults *default-pathname-defaults*))
;; (defun os-realpath (path)
;; (defun implementation-signature ()

(format t "~a~%" (run-suite 'tools-suite :stop-on-fail nil :report-progress t))
