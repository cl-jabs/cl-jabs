;; 1. run tests
;; 2. aggregate results to: stdout, to text file, to...

;; CONSULT: http://maven.apache.org/surefire/maven-surefire-report-plugin/

(defpackage test@core@plugin@jabs
  (:use :cl :tools@jabs :re@jabs :jabs))

(in-package :test@core@plugin@jabs)

(make-instance 'jabs::plugin :name :test :type :core :version jabs::+jabs-version+)

(in-package :jabs)

(export '(get-project-test-files
          *jabs-test-fail-on-error*
          ))

(define-plugin-type :test plugin
  (let ((checker t)
        (plugin-package-name
         (eval (append '(concatenate-symbol *jabs-universal-delimiter*)
                       (list (get-plugin-name plugin))
                       (list (get-plugin-type plugin))
                       +jabs-plugin-namespace+))))
    ;; TODO: check for functions format (args and their types)
    (dolist (v '(:run-single-test :run-single-suite :run-all-tests))
      (when (not (try (symbol-function (tosymbol v plugin-package-name))))
        (setf checker nil)))
    checker))

(define-plugin-type :test-report plugin t)

(defvar *jabs-test-suites-to-run* nil)
(defvar *jabs-tests-to-run* nil)
(defvar *jabs-test-fail-on-error* t)

(defvar *jabs-test-report-formats* '(:stdout :text)) ;; :stdout, :stderr, :text... (probably: xml, json etc - via plugins)

;; bind CLI parameter -Dbout=...
(bind-jabs-cli-parameter
 "tests"
 #'(lambda (&rest x)
     (dolist (test (reverse x))
       (pushnew (tokeyword test) *jabs-tests-to-run*))))

(bind-jabs-cli-parameter
 "test-suites"
 #'(lambda (&rest x)
     (dolist (suite (reverse x))
       (pushnew (tokeyword suite) *jabs-test-suites-to-run*))))

(bind-jabs-cli-parameter
 "test-fail-on-error"
 #'(lambda (&rest x)
     (option-set-bool x *jabs-test-fail-on-error* "test-fail-on-error")))

(bind-jabs-cli-parameter
 "test-report-formats"
 #'(lambda (&rest x)
     (dolist (suite (reverse x))
       (pushnew (tosymbol suite) *jabs-test-report-formats*))))

;; process CLI params
(process-jabs-cli-parameter "tests")
(process-jabs-cli-parameter "test-suites")
(process-jabs-cli-parameter "test-fail-on-error")

(defmacro filter-project-test-plugins (project)
  `(filter-project-plugins-by-type ,project :test))

(defgeneric get-project-test-files (project plugin)
  )

(defmethod get-project-test-files ((project project) (plugin plugin))
  "Parse modeline with test-engine:
ex: ;;; -*- TestEngine: fiveam; -*-
or without test engine
The logic is: if you don't have modeline,
than all of your test files acceptable.
Else, you should mark, what test engine do you use
"
  (let* ((skeleton (find-project-skeleton project))
         (skeleton-test-dir (pathname-as-directory
                             (parse-namestring
                              (or (try (car (get-skeleton-test skeleton)))
                                  (try (get-skeleton-test skeleton))
                                  ""))))
         (project-pathname (pathname-as-directory
                            (parse-namestring
                             (or (project-slot-value project 'pathname) "")))))
    ;; (jlog:note "~a" (merge-pathnames skeleton-test-dir project-pathname))
    (labels ((check-files (string files)
               (if (null files) nil
                   (let ((scanstring
                          (try (middle-scan #\- #\-
                                            (middle-scan #\* #\*
                                                         (os-cat (car files)))))))
                     (if (or
                          (null scanstring)
                          (scan (format nil "TestEngine:~a" string) scanstring)
                          (scan (format nil "TestEngine: ~a" string) scanstring))
                         (cons (car files) (check-files string (cdr files)))
                         (check-files string (cdr files)))))))
      (check-files (tostr (concatenate-symbol
                           *jabs-universal-delimiter*
                           (get-plugin-name plugin)
                           (get-plugin-type plugin)) t)
                   (os-find (merge-pathnames
                             (merge-pathnames skeleton-test-dir project-pathname)
                             (os-pwd))
                            :type :file :extension "lisp")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: make test report generators (smth. like in jlog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric run-single-test-with-plugins (name  project)
  )

(defmethod run-single-test-with-plugins (name (project project))
  (let ((plugins (filter-project-test-plugins project)))
    (plugins-api-call-to-true
     :run-single-test plugins name)))

(defgeneric run-single-suite-with-plugins (name project)
  )

(defmethod run-single-suite-with-plugins (name (project project))
  (let ((plugins (filter-project-test-plugins project)))
    (plugins-api-call-to-true
     :run-single-suite plugins name)))

(defgeneric run-all-tests-with-plugins (project)
  )

(defmethod run-all-tests-with-plugins ((project project))
  (let ((plugins (filter-project-test-plugins project)))
    (plugins-api-call-to-true
     :run-all-tests plugins)))

;; defun register-test-report-format
(defgeneric run-tests (project)
  )

(defmethod run-tests ((project project))
  "Run tests, according to preconfiguration"
  (when (and
         (not *jabs-tests-to-run*)
         (not *jabs-test-suites-to-run*))
    (run-all-tests-with-plugins project))
  (when *jabs-test-suites-to-run*
    (dolist (suite *jabs-test-suites-to-run*)
      (run-single-suite-with-plugins suite project)))
  (when *jabs-tests-to-run*
    (dolist (test *jabs-tests-to-run*)
      (run-single-test-with-plugins test project))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extend 'defproject' definition with 'bout' parameter
(bind-project-symbol
 :test
 #'(lambda (x)
     (declare (ignore x))))

;; extend 'defproject' definition with 'bout' parameter
(bind-project-symbol
 :test-report-formats
 #'(lambda (x)
     (check-type x list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -Dtests=test@suite -> test@ -> suite
;; hit@round@bout -> bout -> round@bout -> round@ -> hit@@bout -> hit@round@ -> hit@@


;; TEST running all files from skeleton test directory by default

;; "**/Test*.java" - includes all of its subdirectories and all Java filenames that start with "Test".
;; "**/*Test.java" - includes all of its subdirectories and all Java filenames that end with "Test".
;; "**/*TestCase.java" - includes all of its subdirectories and all Java filenames that end with "TestCase".

;; <include>Sample.java</include>
;; <exclude>**/TestCircle.java</exclude>
;; +regexps

;; mvn -Dtest=TestCircle#mytest test - run single test
;; mvn -Dtest=TestCircle#testOne+testTwo test - run set of tests
;; mvn -Dtest=TestCircle#test* test - run tests by regexp

;; add reports:   Tests run: 2, Failures: 0, Errors: 0, Skipped: 0, Flakes: 1
;; mvn -Dsurefire.rerunFailingTestsCount=2 test - rerun tests


;; API: run-test run-test-file


;; -Dtest=test@file
;; -Dtest=@file
