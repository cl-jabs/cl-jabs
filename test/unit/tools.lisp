(load (make-pathname :directory '(:relative "test" "unit") :name "header.lisp" :type "in"))

(load (make-pathname :directory '(:relative "src") :name "jabs-tools" :type "lisp"))

(defpackage tools-test
  (:use :cl :test-engine :clunit :tools@jabs))

(in-package :tools-test)

(defun hash-table-test-name (name)
  ;; Workaround for Clisp calling EQL in a hash-table FASTHASH-EQL.
  (hash-table-test (make-hash-table :test name)))

(declaim (notinline opaque))
(defun opaque (x)
  x)

;; tests
(defsuite tools-suite ()
  )

;; :argv
;; :terminate

(deftest maphash-keys-test (tools-suite)
  (let ((keys nil)
	(table (make-hash-table)))
    (declare (notinline maphash-keys))
    (dotimes (i 10)
      (setf (gethash i table) t))
    (maphash-keys (lambda (k) (push k keys)) table)
    (assert-true (set-equal keys '(0 1 2 3 4 5 6 7 8 9)))))

(deftest maphash-values-test (tools-suite)
  (let ((vals nil)
	(table (make-hash-table)))
    (declare (notinline maphash-values))
    (dotimes (i 10)
      (setf (gethash i table) (- i)))
    (maphash-values (lambda (v) (push v vals)) table)
    (assert-true (set-equal vals '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)))))

;; :if-let
(deftest if-let-test (tools-suite)
  (assert-equal :ok (if-let (x (opaque :ok)) x :bad))
  (assert-equal :ok (if-let (x (opaque nil)) :bad (and (not x) :ok)))
  (assert-equal 3 (let ((x 1)) (if-let ((x 2) (y x)) (+ x y) :oops)))
  (assert-equal 1 (if-let ((x 1) (y nil)) :oops (and (not y) x)))
  (assert-equal t (if-let (x) :oops (not x)))
  (assert-equal :type-error (handler-case
			     (eval '(if-let x
					    :oops
					    :oops))
			     (type-error ()
					 :type-error))))

;; :component-present-p
;; :directory-pathname-p
;; :pathname-as-directory
;; :pathname-as-file
;; :directory-wildcard
;; :list-directory
;; :file-exists-p
;; :directory-exists-p
;; :copy-file
;; :lisp-version-string
;; ;;
;; :os-hostname
;; :os-find
;; :os-mkdir
;; :os-cp
;; :os-mv
;; :os-rm
;; :os-touch
;; :os-cat
;; :os-cd
;; :os-pwd
;; :os-getenv
;; ;; parser
;; :parse-args
;; :make-argument-parser
;; :add-argument
;; :get-parsed-argument-value
;; ;;
;; :flatten
(deftest flatten-test (tools-suite)
  (assert-equal
   '(1 2 3)
   (flatten '(1 2 3)))
  (assert-equal
   '(1 2 3)
   (flatten '(1 (2) 3)))
  (assert-equal
   '(1 2 3 4 5 6 3)
   (flatten '(1 (2 3 (4 5 6)) 3))))
;; :tolist
(deftest tolist-test (tools-suite)
  (assert-equal
   '(#\t #\e #\s #\t)
   (tolist "test"))
  (assert-equal
   '(1 2 3)
   (tolist '(1 2 3)))
  (assert-equal
   :type-error
   (handler-case
    (tolist #\q)
    (type-error () :type-error))))

;; :tostr ; TODO: function is not usable
;; :start
;; :run
;; :signal-process
;; :process-id
;; :process-input-stream
;; :process-output-stream
;; :process-error-stream
;; :process-status
;; :process-p
;; ;; REST
;; :merge-n-directories
;; :get-cars
(deftest get-cars-test (tools-suite)
  (assert-equal
   '(1 5)
   (get-cars '((1 2 3 4) (5 6 7 8)))))
;; :scan-all-to-list
;; :list-files-recursively
;; ;; namespaces
;; :namespace-subtree-p
;; :add-namespace-subtree
;; :replace-namespace-subtree
;; ))

(format t "~a~%" (run-suite 'tools-suite :stop-on-fail nil :report-progress nil))
