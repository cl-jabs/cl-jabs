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
