;; (defconstant +jabs-version+ "f65ee71-SNAPSHOT")
;; (defconstant +jabs-run-directory+ (os-pwd))
;; (defvar *jabs-local-share-directory* (make-pathname :directory '(:relative "share")))
;; (defvar *jabs-local-config-directory* (make-pathname :directory '(:relative "etc")))
;; (defvar *jabs-local-lib-directory* (make-pathname :directory '(:relative "lib")))
;; (defvar *jabs-local-src-directory* (make-pathname :directory '(:relative "src")))
;; (defvar *jabs-local-directory* (make-pathname :directory '(:relative ".jabs")))
;; (defvar *jabs-source-directory* (parse-namestring "/home/vyo/dev/cl-jabs/src/"))
;; (defvar *jabs-lib-directory* (parse-namestring "/home/vyo/dev/cl-jabs/lib/"))
;; (defvar *jabs-share-directory* (parse-namestring "/home/vyo/dev/cl-jabs/share/"))
;; (defvar *jabs-config-directory* (parse-namestring "/home/vyo/dev/cl-jabs/etc/"))
;; (defvar *jabs-user-directory* (merge-pathnames
;; (defvar *jabs-buildfile* (make-pathname :name "build" :type "jab"))
;; (defconstant +jabs-configfile+ (make-pathname :name "jabs" :type "conf"))
;; (defvar *jabs-output-log* nil)
;; (defvar *jabs-error-log* nil)
;; (defvar *jabs-universal-delimiter* "@")
;; (defvar *post-init-hook* nil
;; (defvar *fail-on-error* nil)
;; (defun load-build-jab (dir)
;; (defun make-jabs-symbol (symbol &optional (package :keyword))
;; (defun concatenate-symbol (delimiter &rest symbols)
;; (defun get-option-suffix (name list &key (test 'eql) (nth 1))
;; (defun remove-with-arg (item list) ;; &key (test 'eql))
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

(load (make-pathname :directory '(:relative "src") :name "jabs-re" :type "lisp"))
(load (make-pathname :directory '(:relative "src") :name "jabs-core" :type "lisp"))

(defpackage core-test
  (:use :cl :test-engine :clunit :core@jabs))

(in-package :core-test)

;; tests
(defsuite core-suite ()
  )

;; (deftest check-log-string-external-symbol (core-suite)
;;   t)

;; :load-jabs
;; :bind-argv
;; :process-argv
;; :defcompiler
;; :get-compiler-name
;; :get-compiler-path
;; :get-compiler-eval-opt
;; :get-compiler-load-opt
;; :get-compiler-default-opts
;; :get-compiler-debug-opt
;; :get-compiler-no-debug-opt
;; :get-compiler-quit
;; :get-compiler-pre-eval
;; :get-compiler-post-eval
;; :get-compiler-quiet-opt

;; :add-hook
(deftest add-hook-test (core-suite)
  (assert-equal
   "test"
   (let ((zzz-hook))
     (add-hook zzz-hook #'(lambda () (format nil "test")))
     (funcall (car zzz-hook)))))
  
;; :run-hook
(deftest run-hook-test (core-suite)
  (assert-false ;; TODO uses 'dolist' now. Make possibility to collect output data
   (let ((zzz-hook))
     (add-hook zzz-hook #'(lambda (x y) (+ x y)))
     (run-hook zzz-hook 1 2))))
     

;; :make-jabs-symbol
(deftest make-jabs-symbol-test (core-suite)
  (let ((smb (make-jabs-symbol "test")))
    (assert-equal
     (find-package :keyword)
     (symbol-package smb))
    (assert-equal
     "TEST"
     (symbol-name smb))))

;; :concatenate-symbol
(deftest concatenate-symbol-test (core-suite)
  (let ((smb (concatenate-symbol "-" "test" "ing")))
    (assert-equal
     (find-package :keyword)
     (symbol-package smb))
    (assert-equal
     "TEST-ING"
     (symbol-name smb))))

;; :get-option-suffix
;; :bind-jabs-cli-parameter
;; :find-compiler
;; :remove-with-arg
;; :parse-complex-string
;; :get-repository-name
;; :get-repository-type
;; :get-repository-url
;; :get-project-repository-names-list

;; :file-exists-p
;; :pathname-as-directory
;; 
;; :pathname-as-directory
;; :list-directory
;; :directory-pathname-p
;; :pathname-as-file
;; :load-build-jab

(format t "~a~%" (run-suite 'core-suite :stop-on-fail nil :report-progress nil))
