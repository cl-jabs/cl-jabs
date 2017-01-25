;; 1. run tests
;; 2. aggregate results to: stdout, to text file, to...

;; CONSULT: http://maven.apache.org/surefire/maven-surefire-report-plugin/

(defpackage test@core@plugin@jabs
  (:use :cl :tools@jabs :re@jabs :jabs))

(in-package :test@core@plugin@jabs)

(make-instance 'jabs::plugin :name :test :type :core :version jabs::+jabs-version+)

(in-package :jabs)

(export '(get-project-test-files))

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

(defvar *jabs-test-suits-to-run* nil)
(defvar *jabs-tests-to-run* nil)

(defvar *jabs-test-report-formats* '(:stdout :text)) ;; :stdout, :stderr, :text... (probably: xml, json etc - via plugins)

;; bind CLI parameter -Dbout=...
(bind-jabs-cli-parameter
 "tests"
 #'(lambda (&rest x)
     (dolist (test (reverse x))
       (pushnew (tosymbol test) *jabs-tests-to-run*))))

(bind-jabs-cli-parameter
 "test-suits"
 #'(lambda (&rest x)
     (dolist (suite (reverse x))
       (pushnew (tosymbol suite) *jabs-test-suits-to-run*))))

(bind-jabs-cli-parameter
 "test-report-formats"
 #'(lambda (&rest x)
     (dolist (suite (reverse x))
       (pushnew (tosymbol suite) *jabs-test-report-formats*))))

;; process CLI params
(process-jabs-cli-parameter "tests")
(process-jabs-cli-parameter "test-suits")

(defmacro filter-project-test-plugins (project)
  `(filter-project-plugins-by-type ,project :test))

(defmethod get-project-test-files ((project project) (plugin plugin))
  "Parse modeline with test-engine:
ex: ;;; -*- TestEngine: fiveam; -*-
or without test engine
The logic is: if you don't have modeline,
than all of your test files acceptable.
Else, you should mark, what test engine do you use
"
  (let* ((skelethon (find-skelethon
                     (or (try (car (project-slot-value project 'skelethon)))
                         (try (project-slot-value project 'skelethon))
                         *jabs-default-skelethon-name*)))
         (skelethon-test-dir (pathname-as-directory
                              (parse-namestring
                               (or (try (car (get-skelethon-test skelethon)))
                                   (try (get-skelethon-test skelethon))
                                   ""))))
         (project-pathname (pathname-as-directory
                            (parse-namestring
                             (or (project-slot-value project 'pathname) "")))))
    ;; (jlog:note "~a" (merge-pathnames skelethon-test-dir project-pathname))
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
                             (merge-pathnames skelethon-test-dir project-pathname)
                             (os-pwd))
                            :type :file :extension "lisp")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: make test report generators (smth. like in jlog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod run-single-test-with-plugins (name (project project))
  (let ((plugins (filter-project-test-plugins project)))
    (plugins-api-call-to-true
     :run-single-test plugins name)))

(defmethod run-single-suite-with-plugins (name (project project))
  (let ((plugins (filter-project-test-plugins project)))
    (plugins-api-call-to-true
     :run-single-suite plugins name)))

(defmethod run-all-tests-with-plugins ((project project))
  (let ((plugins (filter-project-test-plugins project)))
    (plugins-api-call-to-true
     :run-all-tests plugins)))

;; defun register-test-report-format

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


;; ;; TEST running all files from skelethon test directory by default

;; ;; "**/Test*.java" - includes all of its subdirectories and all Java filenames that start with "Test".
;; ;; "**/*Test.java" - includes all of its subdirectories and all Java filenames that end with "Test".
;; ;; "**/*TestCase.java" - includes all of its subdirectories and all Java filenames that end with "TestCase".

;; ;; <include>Sample.java</include>
;; ;; <exclude>**/TestCircle.java</exclude>
;; ;; +regexps

;; ;; mvn -Dtest=TestCircle#mytest test - run single test
;; ;; mvn -Dtest=TestCircle#testOne+testTwo test - run set of tests
;; ;; mvn -Dtest=TestCircle#test* test - run tests by regexp

;; ;; add reports:   Tests run: 2, Failures: 0, Errors: 0, Skipped: 0, Flakes: 1
;; ;; mvn -Dsurefire.rerunFailingTestsCount=2 test - rerun tests


;; ;; API: run-test run-test-file


;; ;; -Dtest=test@file
;; ;; -Dtest=@file


;; (defmethod register-test (name (project project) &rest body)
;;   (let ((real-test-name (concatenate-symbol "@" name (get-project-name project))))
;;     (if (null (gethash real-test-name *jabs-tests-registry*))
;;         (setf (gethash real-test-name *jabs-tests-registry*) body)
;;       (msg :err "Test ~a at project ~a already exists!" name (get-project-name project)))))

;; (defmacro deftest (name &body body)
;;   `(register-test ,name *current-project* ,@body))

;; (defmethod load-test-file (name (project project))
;;   (check-type name string)
;;   (msg :msg "SKELETHON: ~a, ~a" (get-project-skelethon project) name)
;;   (let ((skel (or (find-skelethon (get-project-skelethon project))
;;                   (find-skelethon (car (get-project-skelethon project)))))
;;         (*current-project* project))
;;     (if (not (null skel))
;;         (let* ((testpath (get-skelethon-test skel))
;;                (load-path
;;                 (merge-pathnames
;;                  (merge-pathnames
;;                   (merge-pathnames (make-pathname :name name :type "lisp")
;;                                    (pathname-as-directory (parse-namestring (car testpath))))
;;                   (pathname-as-directory (parse-namestring (get-project-pathname project))))
;;                  (os-pwd))))
;;           (if (not (null testpath))
;;               (with-info ("Loading ~a" load-path)
;;                          (if (file-exists-p load-path)
;;                              (load load-path)
;;                            (msg :err "Test file ~a does not exists" load-path)))
;;             (msg :warn "Test pathname is not set for skelethon ~a in project ~a"
;;                  (get-skelethon-name skel)
;;                  (get-project-name project))))
;;       (msg :warn "Skelethon is not set for project ~a. Can not load tests" (get-project-name project)))))

;; (defmethod run-test (name (project project))
;;   (msg :warn "=======================================TESTS3: ~a" *jabs-tests-to-run*)
;;   (cond ((null (ppcre:scan "@" name))
;;          (load-test-file name project)
;;          (maphash #'(lambda (x y)
;;                       (when (string-equal
;;                              (ppcre:regex-replace "^.*@" x "")
;;                              (get-project-name project))
;;                         (msg :msg "Launching test ~a..." x)
;;                         (funcall y))) *jabs-tests-registry*))
;;         ((< 2 (length (scan-all-to-list "@" name)))
;;          (msg :err "Incorrect test name format ~a " name))
;;         (t
;;          (load-test-file (ppcre:regex-replace "^.*@" (princ-to-string name) "") project)
;;          (when (string-equal
;;                 (ppcre:regex-replace "^.*@" name "")
;;                 (get-project-name project))
;;            (msg :msg "Launching test ~a..." name)
;;            (funcall (gethash name *jabs-tests-registry*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defpackage depmanager@core@plugin@jabs
;;   (:use :cl :jabs))

;; (in-package :depmanager@core@plugin@jabs)

;; (make-instance 'jabs::plugin :name :depmanager :type :core :version jabs::+jabs-version+)

;; (in-package :jabs)

;; (define-plugin-type :repository plugin
;;   (let ((checker t)
;;         (plugin-package-name
;;          (eval (append '(concatenate-symbol *jabs-universal-delimiter*)
;;                        (list (get-plugin-name plugin))
;;                        (list (get-plugin-type plugin))
;;                        +jabs-plugin-namespace+))))
;;     ;; TODO: check for functions format (args and their types)
;;     (dolist (v '(:initialize-repository :find-repository-project
;;                  :load-repository-project :remove-repository-project
;;                  :repository-project-version :update-project-repository-projects
;;                  :repository-project-dependencies))
;;       (when (not (try (symbol-function (tosymbol v plugin-package-name))))
;;         (setf checker nil)))
;;     checker))

;; (defvar *jabs-global-projects-map* (make-hash-table)
;;   "Define hash with project/file keys")

;; (defvar *jabs-global-systems-map* (make-hash-table)
;;   "Define hash with system/file keys. For ASDF compatability")

;; (defun map-global-projects-to-files ()
;;   "Register map with projects from global paths"
;;   (let ((sources
;;          (append
;;           (os-find
;;            (merge-pathnames (make-pathname :directory '(:relative "lib"))
;;                             *jabs-lib-directory*)
;;            :name "build" :extension "jab"))))
;;     (dolist (file sources)
;;       (dolist (l (os-cat file :list))
;;         (when (eq (tosymbol (car l)) :defproject)
;;           (if (atom (cadr l))
;;               (progn
;;                 (jlog:dbg "Mapping project ``~a'' to file ``~a''" (tosymbol (cadr l)) file)
;;                 (setf (gethash (tosymbol (cadr l)) *jabs-global-projects-map*) file))
;;               (jlog:wrn "Incorrect project format in file ``~a''. Skipping" file)))))))

;; (defun map-global-systems-to-files ()
;;   "Register map with systems from global paths
;; Created for ASDF compatability"
;;   (let ((sources
;;          (append
;;           (os-find
;;            (merge-pathnames (make-pathname :directory '(:relative "lib"))
;;                             *jabs-lib-directory*)
;;            :extension "asd"))))
;;     (dolist (file sources)
;;       (dolist (l (os-cat file :list))
;;         (when (eq (tosymbol (car l)) :defsystem)
;;           (if (atom (cadr l))
;;               (progn
;;                 (jlog:dbg "Mapping asdf system ``~a'' to file ``~a''" (tosymbol (cadr l)) file)
;;                 (setf (gethash (tosymbol (cadr l)) *jabs-global-systems-map*) file))
;;               (jlog:wrn "Incorrect system format in file ``~a''. Skipping" file)))))))

;; ;; register global projects and systems
;; (add-hook *post-init-hook* #'map-global-projects-to-files)
;; (add-hook *post-init-hook* #'map-global-systems-to-files)

;; (defmethod get-project-projects-map ((project project))
;;   "Get project names mapped to files for only for given project
;; w\o global map"
;;   (let* ((skel-name (or (car (try (slot-value project 'skelethon)))
;;                         *jabs-default-skelethon-name*))
;;          (skel (or
;;                 (find-skelethon skel-name)
;;                 (and
;;                  (load-skelethon skel-name)
;;                  (find-skelethon skel-name))))
;;          (lib (or (try (car (get-skelethon-lib skel)))
;;                   (get-skelethon-lib skel)))
;;          (contrib (or (try (car (get-skelethon-contrib skel)))
;;                       (get-skelethon-contrib skel)))
;;          (opt (get-skelethon-opt skel))
;;          (hash (make-hash-table)))
;;     ;;
;;     (mapcar #'(lambda (x)
;;                 (dolist (f
;;                           (os-find (merge-pathnames (parse-namestring x) +jabs-run-directory+)
;;                                    :name "build" :extension "jab"))
;;                   (dolist (v (os-cat f :list))
;;                     (when (eq (tosymbol (car v)) :defproject)
;;                       (setf (gethash (tosymbol (cadr v)) hash) f)))))
;;             (cons lib (cons contrib opt)))
;;     hash))

;; (defmethod get-project-systems-map ((project project))
;;   "Get asdf system names mapped to files for only for given project
;; w\o global map. For ASDF back compatability"
;;   (let* ((skel-name (or (car (try (slot-value project 'skelethon)))
;;                         *jabs-default-skelethon-name*))
;;          (skel (or
;;                 (find-skelethon skel-name)
;;                 (and
;;                  (load-skelethon skel-name)
;;                  (find-skelethon skel-name))))
;;          (lib (or (try (car (get-skelethon-lib skel)))
;;                   (get-skelethon-lib skel)))
;;          (contrib (or (try (car (get-skelethon-contrib skel)))
;;                       (get-skelethon-contrib skel)))
;;          (opt (get-skelethon-opt skel))
;;          (hash (make-hash-table)))
;;     ;;
;;     (mapcar #'(lambda (x)
;;                 (dolist (f
;;                           (os-find (merge-pathnames (parse-namestring x) +jabs-run-directory+)
;;                                    :extension "asd"))
;;                   (dolist (v (os-cat f :list))
;;                     (when (eq (tosymbol (car v)) :defsystem)
;;                       (setf (gethash (tosymbol (cadr v)) hash) f)))))
;;             (cons lib (cons contrib opt)))
;;     hash))

;; (defmethod get-project-depends-on ((project project))
;;   (try (slot-value project 'depends-on)))

;; (defmethod dependency-project-reachable-locally-p (name (project project))
;;   (check-type name keyword)
;;   (or (gethash name (get-project-projects-map project))
;;       (gethash name *jabs-global-projects-map*)))

;; (defmethod dependency-system-reachable-locally-p (name (project project))
;;   (check-type name keyword)
;;   (or (gethash name (get-project-systems-map project))
;;       (gethash name *jabs-global-systems-map*)))

;; (defmethod find-project-dependency-force-locally (name (project project))
;;   "Like ``find-project'', but tries to load local project or
;; system file, than retry to find project"
;;   (let* ((found-project (find-project name))
;;          (found-project-file
;;           (when (null found-project)
;;             (or (dependency-project-reachable-locally-p name project)
;;                 (dependency-system-reachable-locally-p name project)))))
;;     (or found-project
;;         (when found-project-file
;;           (load found-project-file)
;;           (setf found-project (find-project name))
;;           (if (null found-project)
;;               (jlog:crit "Incorrect format of project file ``~a''" found-project-file)
;;               found-project)))))

;; (defmethod dependency-project-reachable-remotely-p (name (project project))
;;   "Check if project can be reachable in all accessible plugins"
;;   (let ((plugins (filter-project-plugins-by-type project :repository)))
;;     (plugins-api-call-to-true
;;      :find-repository-project plugins name)))

;; (defmethod find-project-dependencies-remotely (name (project project))
;;   "Check if project can be reachable in all accessible plugins"
;;   (let ((plugins (filter-project-plugins-by-type project :repository)))
;;     (plugins-api-call-to-true
;;      :repository-project-dependencies plugins name)))

;; (defmethod load-project-remote-dependency (name (project project))
;;   "Check if project can be reachable in all accessible plugins"
;;   (let ((plugins (filter-project-plugins-by-type project :repository)))
;;     (plugins-api-call-to-true
;;      :load-repository-project plugins name)))

;; (defmethod dependency-project-reachable-p (name (project project))
;;   (or (dependency-project-reachable-locally-p name project)
;;       (dependency-system-reachable-locally-p name project)
;;       (dependency-project-reachable-remotely-p name project)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defmethod make-project-depencencies-list ((project project))
;;   "Make full list of project dependencies (recursively)"
;;   (labels
;;       ((make-deps-list (name &key other-deps collector)
;;          (let* ((me (find-project-dependency-force-locally name project))
;;                 (me-deps ;; FIXME: do not load local project to get deps.
;;                  (if me  ;; Just use os-cat
;;                      (get-project-depends-on me)
;;                      (if (dependency-project-reachable-remotely-p name project)
;;                          (find-project-dependencies-remotely name project)
;;                          (jlog:crit "Can not reach dependency ``~a'' for project ``~a'' in any way. Can not resolve dependencies"
;;                                     name (get-project-name project)))))
;;                 (all-deps (append other-deps me-deps)))
;;            ;;
;;            (cond ((null all-deps)
;;                   (cons name collector))
;;                  (t
;;                   (make-deps-list (car all-deps) :other-deps (cdr all-deps) :collector (cons name collector)))))))
;;     (remove (get-project-name project)
;;             (reverse (remove-duplicates (reverse (make-deps-list (get-project-name project))))))))

;; (defmethod load-project-local-dependency (name (project project))
;;   (jlog:dbg "Loading project ``~a'' as dependency for project ``~a''" name (get-project-name project))
;;   (and (find-project-dependency-force-locally name project)
;;        (asdf:operate 'asdf:load-op name)))

;; (defmethod load-project-dependencies ((project project))
;;   (dolist (v (make-project-depencencies-list project))
;;     (or
;;      (load-project-local-dependency (tosymbol v) project)
;;      (load-project-remote-dependency (tosymbol v) project)
;;      (jlog:crit "Can not load dependency ``~a'' for project ``~a'' in any way"
;;                 v (get-project-name project)))))

;; (add-hook *pre-load-plugin-hook*
;;   #'(lambda (x)
;;       ;; we need special behavior for plugins.
;;       ;; Plugin requires to load it's dependencies
;;       ;; before plugin loading.
;;       (let* ((name (get-plugin-name x))
;;              (type (get-plugin-type x))
;;              (plugin-package-name
;;               (eval
;;                (append
;;                 `(concatenate-symbol *jabs-universal-delimiter* ,name ,type)
;;                 +jabs-plugin-namespace+)))
;;              (project (find-project plugin-package-name)))
;;         (jlog:dbg "Loading plugin ``~a'' dependencies"
;;                   (concat-keywords-w-delimiter *jabs-universal-delimiter* name type))
;;         (load-project-dependencies project))))
