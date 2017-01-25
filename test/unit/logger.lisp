(load (make-pathname :directory '(:relative "test" "unit") :name "header.lisp" :type "in"))

(defpackage logger-test
  (:use :cl :test-engine :clunit :logger@jabs))

(in-package :logger-test)

(defvar *mock-msg* "")

(defun check-msg (msg)
  (when (string-contain-p *mock-msg* msg) t))

(setf jlog:*log-string* "#Y-#m-#d #H:#M:#S #L: #R")

(define-output-type :mock-type (date type title msg)
  (setf *mock-msg* (make-log-string date type title msg *log-string*)))

(enable-output-type :mock-type)
(disable-output-type :terminal)

;; tests
(defsuite logger-suite ()
  )

(deftest check-log-string-external-symbol (logger-suite)
  (assert-true (eq (nth-value 1 (find-symbol "*LOG-STRING*" :logger@jabs))
                   :external)))

(deftest logger-test-messages (logger-suite)
  (assert-true (progn (err "test")
                      (check-msg "ERROR: test")))
  (assert-false (progn (wrn "test")
                       (check-msg "WARNING: test")))
  (assert-true (progn
                 (setf *log-level* "WARNING")
                 (wrn "test")
                 (check-msg "WARNING: test")))
  (assert-false (progn
                 (setf *log-disable-notes* t)
                 (note "test")
                 (check-msg "NOTE: test"))))

(format t "~a~%" (run-suite 'logger-suite :stop-on-fail nil :report-progress nil))
