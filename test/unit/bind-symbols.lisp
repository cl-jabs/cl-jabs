(load (make-pathname :directory '(:relative "test" "unit") :name "header.lisp" :type "in"))

(load (make-pathname :directory '(:relative "src" ) :name "jabs-re" :type "lisp"))
(load (make-pathname :directory '(:relative "src" ) :name "jabs-tools" :type "lisp"))
(load (make-pathname :directory '(:relative "src" ) :name "jabs-core" :type "lisp"))

(defpackage bind-symbols-test
  (:use :cl :clunit))

(in-package :bind-symbols-test)


(load (make-pathname :directory '(:relative "src" ) :name "jabs-project" :type "lisp"))

(setf clunit:*clunit-report-format* :tap)

(defsuite bind-symbols-suite ()
  )

;; True (as symbol)
(deftest bind-symbols-test-symbol (bind-symbols-suite)
  (let ((symbol :test))
    (assert-true (jabs::bind-project-symbol symbol #'(lambda(&rest x) (declare (ignore x)) t)))
    (assert-true (gethash (symbol-name symbol) jabs::*jabs-project-bound-symbols*))
    ))

;; False (as string)
(deftest bind-symbols-test-string (bind-symbols-suite)
  (let ((symbol "test"))
    (assert-false (ignore-errors
                    (jabs::bind-project-symbol
                     symbol
                     #'(lambda (&rest x) (declare (ignore x)) t)))) ;; not working with assert-fail for some reason. ignore-errors needed
    (assert-false (gethash symbol jabs::*jabs-project-bound-symbols*))
    ))

(format t "~a~%" (run-suite 'bind-symbols-suite :stop-on-fail nil :report-progress nil))
