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
(in-package :logger@jabs)

(defvar *current-output-registry* nil)

(defvar *log-string* "#Y-#m-#d #H:#M:#S [#L]: #R"
  "
date - yyyy-mm-dd-hh-mm-ss-<TZ>
#Y - year in format yyyy
#y - year in format yy
#m - month in format mm
#b - month in human-readable format
#d - day
#H - hour
#M - minute
#S - second
#s - seconds since 1970 (universal time)
#z - timezone
#u - day of week
#a - day of week in human-readable format
#j - day ot the year
#T - title
#L - loglevel
#R - reason (log message)

;; loglevels - ciritical, error, warning, info, debug
")

(defvar *month-names* '("Jan" "Feb" "Mar"
                        "Apr" "May" "Jun"
                        "Jul" "Aug" "Sep"
                        "Oct" "Nov" "Dec"))

(defvar *day-of-week-names* '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defvar *log-disable-notes* nil)
(defvar *log-level* "ERROR")
(defvar *log-quiet-p* nil)
(defvar *log-levels* '("CRITICAL" "ERROR" "WARNING" "INFO" "DEBUG"))
(defvar *log-trace-p* t)

(defvar *fail-on-critical* t)
(defvar *fail-on-error* nil)

(defun terminate (status)
  (when (not (eq status 0))
    (format *error-output* "KNOCKED OUT!~%"))
  #+sbcl (sb-ext:exit :code status) ; SBCL
  #+ccl (ccl:quit status) ; Clozure CL
  #+clisp( ext:quit status)                 ; GNU CLISP
  #+cmu( unix:unix-exit status)                 ; CMUCL
  #+abcl( ext:quit :status status)         ; Armed Bear CL
  #+allegro( excl:exit status :quiet t)        ; Allegro CL
  #-(or sbcl cmu clisp abcl allegro ecl ccl)(cl-user::quit))

;; loglevels: debug info warning error critical
;; also: level ``note`` which displays in all modes, except quiet
(defmacro nth-date-value (date n &optional timezone)
  `(nth ,n (multiple-value-list (decode-universal-time ,date ,timezone))))

(defmacro pretty-date (date nth &optional timezone)
  `(let ((date (nth-date-value ,date ,nth (if (numberp ,timezone) (* -1 ,timezone) nil))))
     (format nil "~2,'0D" date)))

(defun date-get-timezone (date)
  (let ((zone (nth-date-value date 8)))
    (* -1 zone)))

(defun date-get-year (date &key (short nil) timezone)
  (let* ((zone (or timezone (date-get-timezone date)))
         (year (princ-to-string (nth-date-value date 5 zone))))
    (if short
        (concatenate 'string (cddr (concatenate 'list year)))
      year)))

(defun date-get-year-short (date &key timezone)
  (date-get-year date :short t :timezone timezone))

(defun date-get-month (date &key timezone human-readable-p)
  (let ((month (pretty-date date 4 timezone)))
    (if human-readable-p
        (nth (- (parse-integer month) 1) *month-names*)
      month)))

(defun date-get-month-hr (date &key timezone)
  (date-get-month date :timezone timezone :human-readable-p t))

(defun date-get-day (date &key timezone)
  (pretty-date date 3 timezone))

(defun date-get-hour (date &key timezone)
  (pretty-date date 2 timezone))

(defun date-get-minute (date &key timezone)
  (pretty-date date 1 timezone))

(defun date-get-second (date &key timezone)
  (pretty-date date 0 timezone))

(defun date-get-doy (date &key timezone)
  "Get day-of-the-year"
  (let* ((day (nth-date-value date 3 timezone))
         (month (nth-date-value date 4 timezone))
         (year (nth-date-value date 5 timezone)))
    (loop for m from 2 to month
       do (incf day
                (elt
                 (multiple-value-list
                  (decode-universal-time
                   (- (encode-universal-time 0 0 0 1 m year)
                      (* 60 60 24))))
                 3)))
    (princ-to-string day)))

(defun date-get-dow (date &key timezone human-readable-p)
  "Get day-of-week"
  (let ((dow (parse-integer (pretty-date date 6 timezone))))
    (if human-readable-p
        (nth dow *day-of-week-names*)
      (princ-to-string (+ 1 dow)))))

(defun date-get-dow-hr (date &key timezone)
  (date-get-dow date :timezone timezone :human-readable-p t))

(defvar *date-symbols* (make-hash-table :test 'equal))

(setf (gethash #\Y *date-symbols*) #'date-get-year)
(setf (gethash #\y *date-symbols*) #'date-get-year-short)
(setf (gethash #\m *date-symbols*) #'date-get-month)
(setf (gethash #\b *date-symbols*) #'date-get-month-hr)
(setf (gethash #\d *date-symbols*) #'date-get-day)
(setf (gethash #\H *date-symbols*) #'date-get-hour)
(setf (gethash #\M *date-symbols*) #'date-get-minute)
(setf (gethash #\S *date-symbols*) #'date-get-second)
(setf (gethash #\s *date-symbols*) #'get-universal-time)
(setf (gethash #\z *date-symbols*) #'date-get-timezone)
(setf (gethash #\u *date-symbols*) #'date-get-dow)
(setf (gethash #\a *date-symbols*) #'date-get-dow-hr)

(defun make-log-string (date level title message &optional (format *log-string*))
  (labels
      ((chars-replace (date list)
         (cond ((null list) nil)
               ((char-equal (car list) #\#)
                (cond ((char-equal (cadr list) #\T)
                       (append (concatenate 'list title) (chars-replace date (cddr list))))
                      ((char-equal (cadr list) #\L)
                       (append (concatenate 'list level) (chars-replace date (cddr list))))
                      ((char-equal (cadr list) #\R)
                       (append (concatenate 'list message) (chars-replace date (cddr list))))
                      (t
                       (let ((func (gethash (cadr list) *date-symbols*)))
                         (if func
                             (append (concatenate 'list (funcall func date))
                                     (chars-replace date (cddr list)))
                             (cl:error "Incorrect date format ~a" format))))))
               (t (cons (car list) (chars-replace date (cdr list)))))))
    (format nil (concatenate 'string (chars-replace date (concatenate 'list format))))))

(defvar *log-output-types* (make-hash-table))
(defvar *log-used-output-types* nil)
(defvar *log-file* "/tmp/jabs.log")

(defclass output-type ()
  ((minlevel :accessor ot-maxlevel :initarg :minlevel)
   (format :accessor ot-format :initarg :format)
   (body :accessor ot-body :initarg :body)))

(defun check-output-type-args (args)
  (and (typep args 'list)
       (eq 4 (length args))))

(deftype output-type-args ()
  '(satisfies check-output-type-args))

;; EXAMPLE: HOWTO define output type
;; (define-output-type :sometype (date loglevel title message)
;;   (format t "universal date: ~D, loglevel: ~a, title: ~a, message: ~a~%" date loglevel title message))
;;
;; with pretty date:
;; (define-output-type :othertype (date level title msg)
;;   (format t "~a~%" (make-log-string date level title msg *log-string*)))


(defmacro define-output-type (name args function) ;; TODO:!!!!!!!!!!!
  ;; `(check-type ,args output-type)
  `(let ((instance (make-instance 'output-type :minlevel *log-level*)))
     (setf (ot-body instance) (lambda ,args ,function))
     (setf (gethash ,name *log-output-types*) instance)))

(defmacro enable-output-type (name)
  `(check-type name symbol)
  `(if (gethash ,name *log-output-types*)
       (pushnew ,name *log-used-output-types*)
       (cl:error "No such output type ~a" ,name)))

(defmacro disable-output-type (name)
  `(check-type name symbol)
  `(if (gethash ,name *log-output-types*)
       (setf *log-used-output-types* (remove ,name *log-used-output-types*))
       (cl:error "No such output type ~a" ,name)))

(defun level> (level1 level2)
  (when (< (length (member level1 *log-levels* :test 'equal))
           (length (member level2 *log-levels* :test 'equal))) t))

(defun level< (level1 level2)
  (when (> (length (member level1 *log-levels* :test 'equal))
           (length (member level2 *log-levels* :test 'equal))) t))

(defun log-write-output (level title string)
  "Writes log message to all enabled output types"
  (dolist (otype *log-used-output-types*)
    (let ((ot (gethash otype *log-output-types*)))
      ;; (format t "Level: ~a, OT-Maxlevel: ~a, Loglevel: ~a~%" level (ot-maxlevel ot) *log-level*)
      (if (or
           ;; (and
           ;; (or (equal level (ot-maxlevel ot)) (level< level (ot-maxlevel ot)))
           (or (equal level *log-level*) (level< level *log-level*))
           (and (equal "NOTE" level) (not *log-disable-notes*))) ;; enable-disable notes
          (apply (ot-body ot) (list (get-universal-time) level title string))))))

(defmacro write-output (level title string)
  `(let ((str (if (stringp ',string) '(format nil ,string) '(format nil ,@string))))
     (log-write-output ,level ,title (eval str))))


(define-output-type :terminal (date level title msg)
  (format t "~a~%" (make-log-string date level title msg *log-string*)))

(define-output-type :logfile (date type title msg)
  (with-open-file (s *log-file* :direction :output :if-exists :append :if-does-not-exist :create)
    (format s "~a~%" (make-log-string date type title msg *log-string*))))

(enable-output-type :terminal)
;; (enable-output-type :logfile)

(defmacro crit (&rest message)
  `(let ((str (format nil ,@message)))
     (log-write-output "CRITICAL" "" str)
     (when *fail-on-critical*
       (progn
	 (when *log-trace-p*
	   (sb-debug:print-backtrace))
	 (write-output "CRITICAL" "" "Exiting.")
	 (terminate 1)))))

(defmacro err (&rest message)
  `(let ((str (format nil ,@message)))
     (log-write-output "ERROR" "" str)
     (if *fail-on-error*
         (progn
           (when *log-trace-p*
             (sb-debug:print-backtrace))
           (write-output "ERROR" "" "Exiting.")
           (terminate 1)))))

(defmacro wrn (&rest message)
  `(let ((str (format nil ,@message)))
     (log-write-output "WARNING" "" str)))

(defmacro info (&rest message)
  `(let ((str (format nil ,@message)))
     (log-write-output "INFO" "" str)))

(defmacro dbg (&rest message)
  `(let ((str (format nil ,@message)))
     (log-write-output "DEBUG" "" str)))

(defmacro note (&rest message)
  `(let ((str (format nil ,@message)))
     (log-write-output "NOTE" "" str)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro crit-w-title (title &rest message)
  `(let ((str (format nil ,@message)))
     (log-write-output "CRITICAL" ,title str)
     (if *fail-on-critical*
         (progn
           (write-output "CRITICAL" ,title "Exiting.")
           (terminate 1)))))

(defmacro err-w-title (title &rest message)
  `(let ((str (format nil ,@message)))
     (log-write-output "ERROR" ,title str)
     (if *fail-on-error*
         (progn
           (write-output "ERROR" ,title "Exiting.")
           (terminate 1)))))

(defmacro warn-w-title (title &rest message)
  `(let ((str (format nil ,@message)))
     (log-write-output "WARNING" ,title str)))

(defmacro info-w-title (title &rest message)
  `(let ((str (format nil ,@message)))
     (log-write-output "INFO" ,title str)))

(defmacro dbg-w-title (title &rest message)
  `(let ((str (format nil ,@message)))
     (log-write-output "DEBUG" ,title str)))

(defmacro note-w-title (title &rest message)
  `(let ((str (format nil ,@message)))
     (log-write-output "NOTE" ,title str)))

(defun get-stamp (&optional (stamp "STAMP"))
  (concatenate 'string (princ-to-string (get-universal-time)) "-" (princ-to-string (gensym stamp))))
