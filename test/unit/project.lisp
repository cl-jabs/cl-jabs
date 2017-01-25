(load (make-pathname :directory '(:relative "test" "unit") :name "header.lisp" :type "in"))

(load (make-pathname :directory '(:relative "src") :name "jabs-core" :type "lisp"))
(load (make-pathname :directory '(:relative "src") :name "jabs-project" :type "lisp"))

(defpackage project-test
  (:use :cl :test-engine :clunit :jabs))

(in-package :project-test)

(setf jlog:*log-level* "ERROR")
(setf jlog:*fail-on-error* nil)
(setf jlog:*fail-on-critical* nil)
(setf jlog:*log-disable-notes* t)

;; tests
(defsuite project-suite ()
  )

(defvar *project-exists-name* :exists)
(defvar *project-exists-version* "0.0.0")
(defvar *project-not-exists-name* :not-exists)
(defvar *project-symbols-list* '(:name :long-name :description :long-description
				       :license :repositories :version
				       :pathname :depends-on :author :maintainer
				       :homepage :bug-tracker :mailto :source-control
				       :serial :bout :components :hits :source))

(dolist (v *project-symbols-list*)
  (bind-project-symbol v #'(lambda (&rest x) (declare (ignore x)) t)))

(defproject :just-mock
  :name "JUST-MOCK"
  :version *project-exists-version*
  :components ((:file "test")))

(defproject *project-exists-name*
  :name *project-exists-name*
  :version *project-exists-version*
  :components ((:file "test")))

(defvar *exists-project* (car jabs::*jabs-project-registry*))

(deftest find-project-test (project-suite)
  (assert-true (eq (find-project *project-exists-name*) *exists-project*))
  (assert-false (find-project *project-not-exists-name*)))

(deftest register-project-test (project-suite)
  (assert-true (jabs::register-project :just-mock-2 '(:name "JUST-MOCK-2" :components ((:file "test")))))
  (assert-true (find-project :just-mock-2))
  (assert-true (jabs::register-project :just-mock-2 '(:name "JUST-MOCK-2" :components ((:file "test"))))))


(deftest defproject-test (project-suite)
  (assert-true (defproject :just-mock-3 :name "JUST-MOCK-2" :components ((:file "test"))))
  (assert-true (find-project :just-mock-3))
  (assert-true (defproject :just-mock-3 :name "JUST-MOCK-2" :components ((:file "test")))))

(deftest run-project-test (project-suite)
  (assert-true (jabs::run-project (find-project *project-exists-name*))))

(format t "~a~%" (run-suite 'project-suite :stop-on-fail nil :report-progress nil))
