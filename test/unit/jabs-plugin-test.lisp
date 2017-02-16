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

(dolist (v '("tools" "re" "core" "cli" "project" "plugin"))
  (load (make-pathname :directory '(:relative "src") :name (concatenate 'string "jabs-" v) :type "lisp")))

(defpackage plugin-test
  (:use :cl :test-engine :clunit :jabs))

(in-package :plugin-test)

;; tests
(defsuite plugin-suite ())

(defsuite filled-plugin-suite (plugin-suite))

(deffixture filled-plugin-suite (@body)
  (flet ((jabs::run-round (&rest x)
           (declare (ignore x))
           t))
    (let ((jabs::*jabs-plugin-registry* (make-hash-table))
          (jabs::*jabs-project-registry* (make-hash-table))
          unregistered-plugin)
      ;;
      (defplugin :just-mock-plugin :generic "0.0"
                 :bout :just-mock-bout
                 :components ((:file "test")))
      ;;
      (defplugin :unreg :generic "0.0")
      (setf unregistered-plugin (gethash :unreg@generic@plugin@jabs jabs::*jabs-plugin-registry*))
      (setf (gethash :unreg@generic@plugin@jabs jabs::*jabs-plugin-registry*) nil)

      ;; (defproject :just-mock-project :plugins (:just-mock-plugin@generic :asdf@core))

      @body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar *exists-plugin* (gethash *plugin-exists-name* jabs::*jabs-plugin-registry*))

;; (defconstant +jabs-plugin-namespace+ '(:plugin :jabs))
;; (defvar *jabs-plugin-registry* (make-hash-table))
;; (defvar *jabs-plugin-type-registry* (make-hash-table))
;; (defvar *jabs-plugin-directories*
;; (defvar *pre-define-plugin-hook* nil)
;; (defvar *define-plugin-hook* nil)
;; (defvar *pre-load-plugin-hook* nil)
;; (defvar *load-plugin-hook* nil)
;; (defvar *jabs-shared-plugin-names* nil
;; (defvar *jabs-cli-plugin-names* nil)
(deftest find-plugin-type-test (plugin-suite)
  (assert-true (find-plugin-type :generic))
  (assert-false (find-plugin-type :mockplugin))
  (assert-condition type-error (find-plugin-type "mock"))
  )

(deftest register-plugin-type-test (plugin-suite)
  (assert-true (register-plugin-type :mock))
  (assert-true (register-plugin-type :mock2 #'(lambda (x) (declare (ignore x)) t)))
  (assert-false (register-plugin-type :mock2 #'(lambda (x) (declare (ignore x)) t))) ; already registered
  (assert-condition type-error (register-plugin-type :mock3 t))
  (assert-condition type-error (register-plugin-type "mock3" #'(lambda (x) (declare (ignore x)) t)))
  )

(deftest make-plugin-package-name-test (plugin-suite)
  (assert-equal :mockplugin@generic@plugin@jabs (jabs::make-plugin-package-name :mockplugin))
  (assert-equal :mockplugin@core@plugin@jabs (jabs::make-plugin-package-name :mockplugin :core))
  (assert-false (jabs::make-plugin-package-name :mockplugin :mocktype))
  (assert-condition type-error (jabs::make-plugin-package-name "mockplugin" :mocktype))
  (assert-condition type-error (jabs::make-plugin-package-name :mockplugin "mocktype")))

(deftest find-plugin-test (filled-plugin-suite)
  (assert-true (find-plugin :just-mock-plugin))
  (assert-true (find-plugin :just-mock-plugin :generic))
  (assert-false (find-plugin :just-mock-plugin :core))
  (assert-condition type-error (find-plugin "just-mock-plugin" :core))
  (assert-condition type-error (find-plugin :just-mock-plugin "core")))

(deftest find-plugin-file-test (plugin-suite)
  ;; TODO: stub
  (assert-equal nil (jabs::find-plugin-file :just-mock-plugin))
  (assert-equal nil (jabs::find-plugin-file :just-mock-plugin :type :generic))
  ;; (assert-equal t (find-plugin-file :just-mock-plugin :filename "build"))
  ;; (assert-equal t (find-plugin-file :just-mock-plugin :filetype "jab"))
  )

;; (deftest register-plugin-test (plugin-suite)
;;   (assert-equal t (register-plugin (plugin plugin))))

;; (deftest share-plugin-test (plugin-suite)
;;   (assert-equal t (share-plugin (plugin plugin))))

(deftest plugin-registered-p-test (filled-plugin-suite)
  (let ((plug (find-plugin :just-mock-plugin)))
    (assert-true (jabs::plugin-registered-p plug))
    (assert-false (jabs::plugin-registered-p unregistered-plugin))))

;; (deftest load-plugin-file-test (plugin-suite)
;;   (assert-equal t (load-plugin-file name &key (type :generic) (filename "build") (filetype "jab"))))

;; (deftest insert-plugin-dir-test (plugin-suite)
;;   (assert-equal t (insert-plugin-dir path)))

;; (deftest add-plugin-dir-test (plugin-suite)
;;   (assert-equal t (add-plugin-dir path)))

;; (deftest load-plugin-test (plugin-suite)
;;   (assert-equal t (load-plugin name &optional (type :generic))))

;; (deftest filter-project-plugins-by-type-test (filled-plugin-suite)
;;   (let ((proj (find-project :just-mock-project))
;;         ;; (plug (find-plugin :just-mock-plugin))
;;         )
;;     (assert-equal t (filter-project-plugins-by-type proj :core))))

(format t "~a~%" (run-suite 'plugin-suite :stop-on-fail nil :report-progress nil))
