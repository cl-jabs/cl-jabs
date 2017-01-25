(load (make-pathname :directory '(:relative "test" "unit") :name "header.lisp" :type "in"))

(load (make-pathname :directory '(:relative "src") :name "jabs-core" :type "lisp"))
(load (make-pathname :directory '(:relative "src") :name "jabs-plugin" :type "lisp"))

(defpackage plugin-test
  (:use :cl :test-engine :clunit :jabs))

(in-package :plugin-test)

(setf jlog:*log-level* "ERROR")
(setf jlog:*fail-on-error* nil)
(setf jlog:*fail-on-critical* nil)
(setf jlog:*log-disable-notes* t)

;; tests
(defsuite plugin-suite ()
  )

(deftest plugin-test (plugin-suite)

;; plugin-type-registered-p ;; (name)
;; register-plugin-type ;; (name)
;; make-plugin-package-name ;; (name &optional (type :generic))
;; plugin ;; ()
;; plugin-update ;; (plugin)
;; plugin-registered-p ;; (plugin)
;; load-plugin ;; (plugin)
;; find-plugin ;; (name &optional (type :generic))
;; register-plugin ;; (plugin)
;; register-plugin ;; ((plugin plugin))
;; initialize-instance ;; :after ((plugin plugin) &key)
;; plugin-registered-p ;; ((plugin plugin))
)

(format t "~a~%" (run-suite 'plugin-suite :stop-on-fail nil :report-progress nil))
