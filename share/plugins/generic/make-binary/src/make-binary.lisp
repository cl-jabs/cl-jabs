;; -*- mode: lisp -*-

(in-package :make-binary@generic@plugin@jabs)

(defvar *make-binary-toplevel-function* nil)
(defvar *make-binary-name* nil)

(defbout :make-binary
  :validate
  :initialize
  :compile
  :make-binary
  )

(defround :make-binary :make-binary)

;; (append-hit :make-binary :compile)

(defhit make-binary () ()
        (if *make-binary-toplevel-function*
            (let* ((toplevel-list (re@jabs:split #\: *make-binary-toplevel-function*))
                   (package (or (find-package (tools@jabs:tokeyword (first toplevel-list))) *package*))
                   (function (find-symbol (string-upcase (car (last toplevel-list))) package))
                   (name (or *make-binary-name*
                             (tostr (get-project-name *jabs-current-project*) t)))
                   (folder (or
                            (pathname-as-directory
                             (parse-namestring
                              (get-skeleton-bin
                               (find-skeleton
                                (car (jabs:project-slot-value *jabs-current-project* 'jabs::skeleton))))))
                            "")))
              ;;
              (when (not function)
                (jlog:crit "Can not find symbol, named ``~a'' for top-level function. Can not create binary" *make-binary-toplevel-function*))
              ;;
              (tools@jabs:os-mkdir folder)
              ;;
              (trivial-dump-core:save-executable
               (merge-pathnames
                (make-pathname :name name)
                folder)
               function)
              )
            (jlog:crit "There is no toplevel function defined. Can not create binary")))

(bind-project-symbol
 :make-binary
 #'(lambda (x)
     (check-type x list)
     (let ((name (cadr (member :name x)))
           (toplevel-function (cadr (member :toplevel-function x))))
       ;;
       (when (not *make-binary-name*)
         (setf *make-binary-name* name))
       (when (not *make-binary-toplevel-function*)
         (setf *make-binary-toplevel-function* toplevel-function)))))

(bind-jabs-cli-parameter
 "make-binary-name"
 #'(lambda (x)
     (setf *make-binary-name* x)))

(bind-jabs-cli-parameter
 "make-binary-toplevel-function"
 #'(lambda (x)
     (setf *make-binary-toplevel-function* x)))

(process-jabs-cli-parameter "make-binary-name")
(process-jabs-cli-parameter "make-binary-toplevel-function")
