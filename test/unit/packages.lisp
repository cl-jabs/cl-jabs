(load (make-pathname :directory '(:relative "test" "unit") :name "header.lisp" :type "in"))

(defpackage packages-test
  (:use :cl :clunit))

(in-package :packages-test)

(load (make-pathname :directory '(:relative "src") :name "jabs-packages" :type "lisp"))

(defsuite packages-suite ()
  )

;; Check packages

(deftest packages-exists (packages-suite)
  (dolist (v '(:logger@jabs :regexp@jabs :tools@jabs :core@jabs :jabs))
    (assert-true (find-package v))))

;; ;; True
;; (deftest packages-test-symbol (packages-suite)
;;   (let ((symbol :test))
;;     (assert-true (jabs::bind-project-symbol symbol #'(lambda(&rest x) (declare (ignore x)) t)))
;;     (assert-true (gethash (symbol-name symbol) jabs::*jabs-project-bound-symbols*))
;;     ))

;; ;; False (as string)
;; (deftest packages-test-string (packages-suite)
;;   (let ((symbol "test"))
;;     (assert-false (ignore-errors
;;                     (jabs::bind-project-symbol
;;                      symbol
;;                      #'(lambda (&rest x) (declare (ignore x)) t)))) ;; not working with assert-fail for some reason. ignore-errors needed
;;     (assert-false (gethash symbol jabs::*jabs-project-bound-symbols*))
;;     ))

(format t "~a~%" (run-suite 'packages-suite :stop-on-fail nil :report-progress nil))
