(load (make-pathname :directory '(:relative "test" "unit") :name "header.lisp" :type "in"))

(load (make-pathname :directory '(:relative "src") :name "jabs-tools" :type "lisp"))
(load (make-pathname :directory '(:relative "src") :name "jabs-core" :type "lisp"))
(load (make-pathname :directory '(:relative "src") :name "jabs-argparse" :type "lisp"))
(load (make-pathname :directory '(:relative "src") :name "jabs-cli" :type "lisp"))

(defpackage cli-test
  (:use :cl :test-engine :clunit :jabs))

(in-package :cli-test)

;; tests
(defsuite cli-suite ()
  )

(deftest dummy-test (cli-suite)
  )

(format t "~a~%" (run-suite 'cli-suite :stop-on-fail nil :report-progress nil))
