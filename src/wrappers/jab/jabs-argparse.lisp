;; ;;; The following lines added by ql:add-to-init-file:
;; #-quicklisp
;; (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                        (user-homedir-pathname))))
;;   (when (probe-file quicklisp-init)
;;     (load quicklisp-init)))

;; (ql:quickload "log4cl")
;; parent parser - parser, which goes after

;; (defvar *args* (make-hash-table :test 'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; COMMON ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :tools@jabs)

;; (defun argv (&optional argnumber)
;;   (let ((argv
;;          (or
;;           #+clisp (ext:argv)
;;           #+sbcl (cdr sb-ext:*posix-argv*)
;;           #+abcl ext:*command-line-argument-list*
;;           #+clozure ccl:*unprocessed-command-line-arguments* ;(ccl::command-line-arguments)
;;           #+gcl si:*command-args*
;;           #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
;;           #+cmu extensions:*command-line-strings*
;;           #+allegro (sys:command-line-arguments)
;;           #+lispworks sys:*line-arguments-list*
;;           nil)))
;;     (if (not (null argnumber))
;;         (nth argnumber argv)
;;       argv)))

(defun check-arg-integer (arg)
  (or (ignore-errors (parse-integer arg))
      (jlog:crit "argument ~a is not an integer" arg)))

(defun check-arg-string (arg)
  arg)

(defun check-store-syntax (args)
  (if (or
       (null (cadr args))
       (eq #\- (car (concatenate 'list (cadr args)))))
      (jlog:crit "Argument ~a expected one argument" (car args))
    t))

(defmacro check-arg-type (name type)
  `(,(intern (string-upcase
              (concatenate 'string "check-arg-" (princ-to-string type)))
             (find-package
        (intern (concatenate 'string "TOOLS" jabs:*jabs-universal-delimiter* "JABS") (find-package :keyword))))
    ,name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; /COMMON ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass argument-parser ()
  ((description :accessor ap-description :initarg :description)
   (help :accessor ap-help :initarg :help)
   (arguments :accessor ap-arguments :initform (make-hash-table :test 'equal) :initarg :arguments)
   (parent :accessor ap-parent)
   (subparsers :accessor ap-subparsers)
   ))

(defclass argument ()
  ((keys :accessor arg-keys :initarg :keys)
   (type :accessor arg-type :initarg :type) ;; int string etc
   (action :accessor arg-action :initarg :action) ;; store, store-const, store-t, store-nil, append, count
   (destination :accessor arg-destination :initarg :destination)
   (default :accessor arg-default :initarg :default)
   (help :accessor arg-help :initarg :help)
   ))

(defgeneric get-arguments-from-parser (parser)
  )

(defmethod get-arguments-from-parser ((parser argument-parser))
  (let ((a-list))
    (maphash #'(lambda (x y) (declare (ignore x)) (push y a-list)) (ap-arguments parser))
    a-list))

(defgeneric get-argument-from-parser (destination parser)
  )

(defmethod get-argument-from-parser (destination (parser argument-parser))
  (gethash destination (ap-arguments parser)))

(defgeneric get-parser-key-arguments (parser)
  )

(defmethod get-parser-key-arguments ((parser argument-parser))
  (let ((k-a-pairs nil))
    (maphash #'(lambda (x y)
                 (declare (ignore x))
                 (dolist (v (arg-keys y))
                   (pushnew (list v y) k-a-pairs)))
             (ap-arguments parser))
    k-a-pairs))

(defun get-argument-from-k-a-list (arg list)
  (cond ((null list) nil)
        ((equal arg (caar list)) (cadar list))
        (t (get-argument-from-k-a-list arg (cdr list)))))

(defgeneric parse-args (parser)
  )

(defmethod parse-args ((parser argument-parser))
  (let* ((list)
         (hash (make-hash-table :test 'equal))
         (args (get-arguments-from-parser parser))
         (arg-keys (flatten (mapcar #'(lambda (x) (arg-keys x)) args)))
         (k-a-list (get-parser-key-arguments parser)))
    (labels ((store-args (arguments)
                         (cond ((null arguments) nil)
                               ((not (member (car arguments) arg-keys :test 'equal))
                                ;; !!!jabs ONLY!!! (for process direct arguments)
                                #+jab
                                (if (not (begin-scan "-D" (car arguments)))
                                    ;; recognize it as hit!
                                    (progn
                                      (pushnew
                                       (car arguments)
                                       jab::*jabs-hits*
                                       :test 'string-equal)
                                      (store-args (cdr arguments)))
                                  (jlog:crit "Unrecognized argument: ~a" (car arguments)))
                                #-jab
                                (jlog:crit "Unrecognized argument: ~a" (car arguments))
                                )
                               #+jab
                               ((begin-scan "-D" (car arguments))
                                (jlog:dbg "~a is direct argument. Skipping" (car arguments))
                                (store-args (cdr arguments)))
                               (t
                                (let ((action (arg-action (get-argument-from-k-a-list (car arguments) k-a-list)))
                                      (argtype (arg-type (get-argument-from-k-a-list (car arguments) k-a-list)))
                                      (dest (arg-destination (get-argument-from-k-a-list (car arguments) k-a-list)))) ;; dest
                                  ;; check argument type
                                  (cond ((and (eq action :store) (check-store-syntax arguments))
                                         (setf (gethash dest hash) (eval `(check-arg-type ,(cadr arguments) ,argtype)))
                                         (store-args (cddr arguments)))
                                        ((eq action :store-t)
                                         (setf (gethash dest hash) t)
                                         (store-args (cdr arguments)))
                                        ((eq action :store-nil)
                                         (setf (gethash dest hash) nil)
                                         (store-args (cdr arguments)))
                                        ((eq action :count)
                                         (let ((num (gethash dest hash)))
                                           (if (null num) (setf num 0))
                                           (setf (gethash dest hash)
                                                 (+ 1 num)))
                                         (store-args (cdr arguments)))
                                        ((eq action :append)
                                         (let ((list (gethash dest hash)))
                                           (setf (gethash dest hash)
                                                 (cons (eval `(check-arg-type ,(cadr arguments) ,argtype)) list)))
                                         (store-args (cddr arguments)))
                                        (t (jlog:crit "Not supported action ~a" action))))))))
      ;; TODO: check for required arguments
      (store-args (argv)))
    (maphash #'(lambda (x y) (push (list x y) list)) hash)
    list))

;; parser.parse_args

;; TODO: add-parser, parse-arguments
(defmacro make-argument-parser (&key description help)
  `(make-instance 'argument-parser :description ,description :help ,help))

(defgeneric add-argument (parser &key &allow-other-keys)
  )

(defmethod add-argument ((parser argument-parser) &key keys (action :store) (type :string) default destination help)
  (check-type keys (and list (not nil)))
  (check-type type symbol)
  (check-type destination string)
  (check-type action symbol) ;; TODO: add type
  (if (null (get-argument-from-parser destination parser))
      (let ((arg-instance
             (make-instance 'argument :keys keys :type type :action action
                            :default default :destination destination :help help)))
        (setf (gethash destination (ap-arguments parser)) arg-instance)
        arg-instance)))

(defun get-parsed-argument-value (destination parsed-args)
  (check-type destination string)
  (check-type parsed-args list)
  (cond ((null parsed-args) nil)
        ((equal destination (caar parsed-args))
         (cadar parsed-args))
        (t (get-parsed-argument-value destination (cdr parsed-args)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; TODO: subparsers
;; ;; TODO: groups
