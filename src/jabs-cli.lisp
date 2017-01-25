;;; -*- Mode: Lisp -*-

(in-package :tools@jabs)

;;;; tools
(defvar *jabs-cli-actions* (make-hash-table :test 'equal))

(defun bind-jabs-cli-parameter (parameter function)
  (check-type parameter (or string symbol))
  (check-type function function)
  (let ((param-symbol (tosymbol parameter)))
    (setf (gethash param-symbol *jabs-cli-actions*) function)))

(defun process-jabs-cli-parameter (parameter)
  (check-type parameter (or string symbol))
  (let ((parameter-symbol (tosymbol parameter))
        (parameter-value))
    ;; get parameter value
    (dolist (x (argv))
      (when (or (not (scan "=" x))
                (not (scan "-D" x)))
        (jlog:crit "Incorrect argument format ~a" x))
      (when (scan (string-downcase (princ-to-string parameter-symbol)) x)
        (setf parameter-value (cadr (split #\= (begin-cut "-D" x))))))
    ;; get parameter action
    (let ((func (gethash parameter-symbol *jabs-cli-actions*)))
      (if (functionp func)
          ;; apply action to parameter value
          (when (and (not (null parameter-value))
                     (not (string-equal "" parameter-value)))
            (if (stringp parameter-value)
                (apply func (split #\, parameter-value))
                (apply func parameter-value)))
          (jlog:crit "Incorrect parameter ~a, options ~a" parameter-symbol parameter-value)))))

(defmacro option-set-bool (argument variable option-name)
  `(cond ((not (or (null (cdr ,argument))
                   (string-equal "t" (car ,argument))
                   (string-equal "T" (car ,argument))
                   (string-equal "nil" (car ,argument))
                   (string-equal "NIL" (car ,argument))))
          (jlog:err "Incorrect <~a> option format defined from CLI: ~a (t or nil needed)" ,option-name (car ,argument)))
         ((or (string-equal "T" (car ,argument)) (string-equal "t" (car ,argument)))
          (setf ,variable t))
         ((or (string-equal "NIL" (car ,argument)) (string-equal "nil" (car ,argument)))
          (setf ,variable nil))))

(in-package :jabs)

;;;; bind params
(bind-jabs-cli-parameter
 "buildfile"
 #'(lambda (&rest x)
     (when (not (null (cdr x)))
       (jlog:crit "Incorrect buildfile format defined from CLI: ``~a''" x))
     (if (file-exists-p (car x))
         (setf *jabs-buildfile* (car x))
         (jlog:crit "buildfile ``~a'' does not exists." (car x)))))

(bind-jabs-cli-parameter
 "quiet"
 #'(lambda (&rest x)
     (option-set-bool x *jabs-quiet* "quiet")
     (when (or (string-equal (car x) "t")
               (string-equal (car x) "T"))
       (setf *jabs-verbose* nil)
       (setf *jabs-debug* nil)
       (setf jlog:*log-level* "CRITICAL")
       (setf jlog:*log-disable-notes* t))))

(bind-jabs-cli-parameter
 "verbose"
 #'(lambda (&rest x)
     (option-set-bool x *jabs-verbose* "verbose")
     (when (or (string-equal (car x) "t")
               (string-equal (car x) "T"))
       (setf *jabs-quiet* nil)
       (setf *jabs-debug* nil)
       (setf jlog:*log-level* "INFO"))))

(bind-jabs-cli-parameter
 "debug"
 #'(lambda (&rest x)
     (option-set-bool x *jabs-debug* "debug")
     (when (or (string-equal (car x) "t")
               (string-equal (car x) "T"))
       (proclaim '(optimize (debug 3)))
       (setf *jabs-verbose* t)
       (setf *jabs-quiet* nil)
			 (setf jlog::*log-trace-p* t)
       (setf jlog:*log-level* "DEBUG"))))

(bind-jabs-cli-parameter
 "fail-on-error"
 #'(lambda (&rest x)
     (option-set-bool x *fail-on-error* "fail-on-error")))

;; (bind-jabs-cli-parameter
;;  "use-bouts"
;;  #'(lambda (&rest x)
;;      (option-set-bool x *jabs-use-bouts* "use-bouts")))

;; (bind-jabs-cli-parameter
;;  "use-hitdeps"
;;  #'(lambda (&rest x)
;;      (option-set-bool x *jabs-use-hit-deps* "use-hitdeps")))

(bind-jabs-cli-parameter
 "logfile"
 #'(lambda (&rest x)
     (when (not (null (cdr x)))
       (jlog:crit "Incorrect <logfile> format defined from CLI: ``~a''" (car x)))
     (setf *jabs-output-log* (car x))))

(bind-jabs-cli-parameter
 "error-logfile"
 #'(lambda (&rest x)
     (when (not (null (cdr x)))
       (jlog:crit "Incorrect <error-logfile> format defined from CLI: ``~a''" (car x)))
     (setf *jabs-error-log* (car x))))

(bind-jabs-cli-parameter ;; we need to break init if "version" option found: special behavior
 "version"
 #'(lambda (&rest x)
     (declare (ignore x))
     (format *error-output* "(Just (Another (Build (System :version ~a))))~%" +jabs-version+)
     (terminate 0)))

;; early process some system parameters
(dolist (param '("quiet" "verbose" "debug" "version" "buildfile" "logfile" "error-logfile"))
  (process-jabs-cli-parameter param))
