(load (make-pathname :directory '(:relative "test" "unit") :name "header.lisp" :type "in"))

(load (make-pathname :directory '(:relative "src" ) :name "jabs-re" :type "lisp"))
(load (make-pathname :directory '(:relative "src" ) :name "jabs-tools" :type "lisp"))
(load (make-pathname :directory '(:relative "src" ) :name "jabs-core" :type "lisp"))
(load (make-pathname :directory '(:relative "src" ) :name "jabs-plugin" :type "lisp"))

(defpackage plugin-definition-test
  (:use :cl :clunit))

(in-package :plugin-definition-test)


;; (setf jlog:*log-level* "DEBUG")
;; (setf jlog:*fail-on-error* t)

(setf clunit:*clunit-report-format* :tap)

(defsuite plugin-definition-suite ()
  )

(defmacro make-plugin-instance (&body body)
  `(let ((instance (make-instance 'jabs::plugin ,@body)))
     (jabs::plugin-registered-p instance)))

;; register plugin
(deftest plugin-definition-test-register-plugin (plugin-definition-suite)
  (let* ((name :test)
         (type :test-type)
         (package-name
          (intern
           (concatenate 'string (symbol-name name) "@" (symbol-name type) "@" "PLUGIN" "@" "JABS")
           (find-package :keyword))))
;;;; True
    (assert-true (eval `(defpackage ,package-name (:use :tools@jabs :core@jabs :jabs :cl))))
    ;; (assert-true (gethash (symbol-name symbol) jabs::*jabs-project-bound-symbols*))
    (assert-true (make-plugin-instance :name name :type type :uri "http://quicklisp.org" :path "/tmp" :version "0.0"))
    (assert-true (make-plugin-instance :name name :type type :uri "http://quicklisp.org" :path "/tmp"))
    (assert-true (make-plugin-instance :name name :type type :uri "http://quicklisp.org"))
    (assert-true (make-plugin-instance :name name :type type))
;;;; False
    (assert-false (ignore-errors (make-plugin-instance :name name)))
    (assert-false (ignore-errors (make-plugin-instance :name (symbol-name name) :type (symbol-name type))))))

;; plugin-registered-p
(deftest plugin-definition-test-plugin-registered-p (plugin-definition-suite)
  (let* ((name :test)
         (type :test-type)
         (package-name
          (intern
           (concatenate 'string (symbol-name name) "@" (symbol-name type) "@" "PLUGIN" "@" "JABS")
           (find-package :keyword)))
         (instance))
    ;; prepare
    (eval `(defpackage ,package-name (:use :tools@jabs :core@jabs :jabs :cl)))
    (setf instance (make-instance 'jabs::plugin :name name :type type :uri "http://quicklisp.org" :path "/tmp" :version "0.0"))
    ;; true
    (assert-true (jabs::plugin-registered-p instance))
    ;; false
    (assert-false (progn (setf (gethash package-name jabs::*jabs-plugin-registry*) nil)
                         (jabs::plugin-registered-p instance)))))

;; find-plugin
(deftest plugin-definition-test-find-plugin (plugin-definition-suite)
  (let* ((name :test)
         (type :test-type)
         (package-name
          (intern
           (concatenate 'string (symbol-name name) "@" (symbol-name type) "@" "PLUGIN" "@" "JABS")
           (find-package :keyword))))
    ;; prepare
    (eval `(defpackage ,package-name (:use :tools@jabs :core@jabs :jabs :cl)))
    (make-plugin-instance :name name :type type)
    ;; true
    (assert-true (jabs::find-plugin name type))
    ;; false
    (assert-false (jabs::find-plugin name))
    (assert-false (ignore-errors (jabs::find-plugin (symbol-name name) (symbol-name type))))))

;; register-plugin-type
(deftest plugin-definition-test-register-plugin-type (plugin-definition-suite)
  (let* ((name :test)
         (type :test-type-for-register)
         (package-name
          (intern
           (concatenate 'string (symbol-name name) "@" (symbol-name type) "@" "PLUGIN" "@" "JABS")
           (find-package :keyword))))
    ;; true
    ;; (assert-true (jabs::register-plugin-type type))
    (assert-true (jabs::register-plugin-type type))
    ;; false
    (assert-false (ignore-errors (jabs::register-plugin-type (symbol-name type))))
    (assert-false (ignore-errors (jabs::register-plugin-type name type)))
    ))

;; plugin-type-registered-p
(deftest plugin-definition-test-plugin-type-registered-p (plugin-definition-suite)
  (let* ((name :test)
         (type :test-type-for-register-2)
         (package-name
          (intern
           (concatenate 'string (symbol-name name) "@" (symbol-name type) "@" "PLUGIN" "@" "JABS")
           (find-package :keyword))))
    ;; true
    (assert-true (jabs::register-plugin-type type))
    ;; false
    (assert-false (ignore-errors (jabs::register-plugin-type (symbol-name type))))
    (assert-false (ignore-errors (jabs::register-plugin-type name type)))
    ))


(format t "~a~%" (run-suite 'plugin-definition-suite :stop-on-fail nil :report-progress nil))
