;;; -*- Mode: Lisp -*-
#|
MIT License

Copyright (c) 2017 Alexander Vynnyk <cosmonaut.ok@zoho.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|#
(in-package :tools@jabs)

;;;; tools
(defvar *jabs-cli-actions* (make-hash-table :test 'equal))

(defun bind-jabs-cli-parameter (parameter function)
  (check-type parameter string) ;; TODO: make it symbol
  (check-type function function)
  (let ((param-symbol (tosymbol parameter)))
    (setf (gethash param-symbol *jabs-cli-actions*) function)))

(defun process-jabs-cli-parameter (parameter)
  (check-type parameter string) ;; TODO: make it symbol
  (let ((parameter-symbol (tosymbol parameter))
        (parameter-value)
        (parameter-found-p))
    ;; get parameter value
    (dolist (x (argv))
      (when (not (scan "-D" x))
        (jlog:crit "Incorrect argument format ``~a''" x))
      (when (string-equal
             (tostr parameter-symbol t)
             (car (split #\= (begin-cut "-D" x))))
        (setf parameter-found-p t)
        (setf parameter-value (cadr (split #\= (begin-cut "-D" x))))))
    ;; get parameter action
    (let ((func (gethash parameter-symbol *jabs-cli-actions*)))
      (if (functionp func)
          ;; apply action to parameter value
          (when parameter-found-p
            (if (stringp parameter-value)
                (apply func (split #\, parameter-value))
                (apply func parameter-value)))
          (jlog:crit "Incorrect parameter ``~a'', options ``~a''"
                     parameter-symbol parameter-value)))))

(defmacro option-set-bool (argument variable option-name)
  `(cond ((not (or (null (cdr ,argument))
                   (string-equal "t" (car ,argument))
                   (string-equal "T" (car ,argument))
                   (string-equal "nil" (car ,argument))
                   (string-equal "NIL" (car ,argument))))
          (jlog:err "Incorrect ``~a'' option format defined from CLI: ``~a'' (t or nil required)"
                    ,option-name (car ,argument)))
         ((or (string-equal "T" (car ,argument)) (string-equal "t" (car ,argument)))
          (setf ,variable t))
         ((or (string-equal "NIL" (car ,argument)) (string-equal "nil" (car ,argument)))
          (setf ,variable nil))))

(defmacro bool-option-p (argument)
  "Check if boolean option set to T or NIL.
We need it to convert it from list with string
to real T/NIL values"
  `(cond ((not (or (null (cdr ,argument))
                   (string-equal "t" (car ,argument))
                   (string-equal "T" (car ,argument))
                   (string-equal "nil" (car ,argument))
                   (string-equal "NIL" (car ,argument))))
          (jlog:err "Incorrect format defined from CLI: ``~a'' (t or nil required)"
                    ,(concatenate-to-string-with-delimiter #\, argument)))
         ((or (string-equal "T" (car ,argument)) (string-equal "t" (car ,argument))) t)
         ((or (string-equal "NIL" (car ,argument)) (string-equal "nil" (car ,argument))) nil)))

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
     (when (bool-option-p x)
       (setf jlog:*log-level* "CRITICAL")
       (setf jlog:*log-disable-notes* t))))

(bind-jabs-cli-parameter
 "verbose"
 #'(lambda (&rest x)
     (when (bool-option-p x)
       (setf jlog:*log-level* "INFO"))))

(bind-jabs-cli-parameter
 "debug"
 #'(lambda (&rest x)
     (when (bool-option-p x)
       (proclaim '(optimize (debug 3)))
			 (setf jlog::*log-trace-p* t)
       (setf jlog:*log-level* "DEBUG"))))

(bind-jabs-cli-parameter
 "fail-on-error"
 #'(lambda (&rest x)
     (option-set-bool x *fail-on-error* "fail-on-error")))

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
     (when x (jlog:crit "You can not call ``version'' with parameter(s) ``~a''" x))
     (format *error-output* "(Just (Another (Build (System :version ~a))))~%" +jabs-version+)
     (terminate 0)))

;; early process some system parameters
(dolist (param '("quiet" "verbose" "debug" "version" "buildfile" "logfile" "error-logfile"))
  (process-jabs-cli-parameter param))
