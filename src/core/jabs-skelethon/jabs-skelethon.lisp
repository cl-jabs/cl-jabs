(defpackage skelethon@core@plugin@jabs
  (:use :cl :tools@jabs :re@jabs :jabs))

(in-package :skelethon@core@plugin@jabs)

(make-instance 'jabs::plugin :name :skelethon :type :core :version jabs::+jabs-version+)

(in-package :jabs)

(export '(defskelethon
          find-skelethon
          ;;           run-skelethon
          skelethon
          ;;
          get-skelethon-name
          get-skelethon-src
          get-skelethon-bin
          get-skelethon-lib
          get-skelethon-doc
          get-skelethon-share
          get-skelethon-test
          get-skelethon-contrib
          get-skelethon-conf
          get-skelethon-public
          get-skelethon-script
          get-skelethon-cache
          get-skelethon-log
          get-skelethon-template
          get-skelethon-opt
          get-skelethon-tmp
	  get-skelethon-target
          get-skelethon-readme-file
          get-skelethon-license-file
          get-skelethon-install-file
          ;;
          *jabs-default-skelethon-name*
          load-skelethon
          ))

(defvar *jabs-skelethon-directories*
  (list
   (merge-pathnames (make-pathname :directory '(:relative "skelethons")) *jabs-share-directory*)
   (merge-pathnames (make-pathname :directory '(:relative ".jabs" "skelethons")) (user-homedir-pathname))
   (merge-pathnames (make-pathname :directory '(:relative ".jabs" "skelethons")) (os-pwd))
   ))

(defvar *jabs-default-skelethon-name* :default)
(defvar *jabs-skelethon-registry* (make-hash-table))
(defvar *jabs-current-skelethon* nil)
(defvar *jabs-skelethon-template-type* "skl")

(defclass skelethon ()
  ((name          :accessor get-skelethon-name          :initarg :name)
   (src           :accessor get-skelethon-src           :initarg :src)
   (bin           :accessor get-skelethon-bin           :initarg :bin)
   (lib           :accessor get-skelethon-lib           :initarg :lib)
   (doc           :accessor get-skelethon-doc           :initarg :doc)
   (share         :accessor get-skelethon-share         :initarg :share)
   (test          :accessor get-skelethon-test          :initarg :test)
   (contrib       :accessor get-skelethon-contrib       :initarg :contrib)
   (conf          :accessor get-skelethon-conf          :initarg :conf)
   (public        :accessor get-skelethon-public        :initarg :public)
   (script        :accessor get-skelethon-script        :initarg :script)
   (cache         :accessor get-skelethon-cache         :initarg :cache)
   (log           :accessor get-skelethon-log           :initarg :log)
   (template      :accessor get-skelethon-template      :initarg :template)
   (opt           :accessor get-skelethon-opt           :initarg :opt) ;; other optional directories, not /opt :)
   (tmp           :accessor get-skelethon-tmp           :initarg :tmp)
   (target        :accessor get-skelethon-target        :initarg :target)
   (readme-file   :accessor get-skelethon-readme-file   :initarg :readme-file)
   (license-file  :accessor get-skelethon-license-file  :initarg :license-file)
   (install-file  :accessor get-skelethon-install-file  :initarg :install-file)))

(defun find-skelethon (name)
  (check-type name keyword)
  (gethash name *jabs-skelethon-registry*))

(defun skelethondir-p (dir)
  "Skelethon directory record must be a string, or list
   in format

 (\"dirname\" 755 :required)
or
 (\"dirname\" :optional)
or
 (\"dirname\" 511)

It`s optional by default
"
  (flet ((ro (symbol)
           "check for :required or :optional"
           (when (or (eq :required symbol) (eq :optional symbol)) t)))
	  (when (or (null dir)
              (stringp dir)
              (pathnamep dir)
              (and (listp dir)
                   ;; 1st arg
                   (or (stringp (car dir))
                       (pathnamep (car dir)))
                   ;; 2nd arg
                   (or (integerp (nth 1 dir))
                       (ro (nth 1 dir)))
                   ;; 3rd arg
                   (or (null (nth 2 dir))
                       (ro (nth 2 dir)))
                   (null (nth 3 dir))))
	    t)))

(defun listskelethondirs-p (dirs)
  "Skelethon can take list of optional directories, which are
not included to main schema (like socket or dev dir etc)"
  (cond ((null dirs) t)
        ((atom dirs) nil)
        (t (when (skelethondir-p (car dirs))
             (listskelethondirs-p (cdr dirs))))))

(deftype skelethondir ()
  '(satisfies skelethondir-p))

(deftype listskelethondirs ()
  '(satisfies listskelethondirs-p))

(defun register-skelethon (name &key bin lib src test conf share doc
                                  contrib public script cache log template opt tmp
                                  target readme-file license-file install-file)
  (check-type name keyword)
  (check-type bin (or null skelethondir))
  (check-type lib (or null skelethondir))
  (check-type src (or null skelethondir))
  (check-type test (or null skelethondir))
  (check-type conf (or null skelethondir))
  (check-type share (or null skelethondir))
  (check-type doc (or null skelethondir))
  (check-type contrib (or null skelethondir))
  (check-type public (or null skelethondir))
  (check-type script (or null skelethondir))
  (check-type cache (or null skelethondir))
  (check-type log (or null skelethondir))
  (check-type template (or null skelethondir))
  (check-type tmp (or null skelethondir))
  (check-type opt (or null listskelethondirs))
  (check-type target (or null skelethondir))
  (check-type readme-file (or null string))
  (check-type license-file (or null string))
  (check-type install-file (or null string))
  (jlog:dbg "Registering skelethon ``~a''" name)
  (setf
   (gethash name *jabs-skelethon-registry*)
   (make-instance
    'skelethon
    :name name
    :bin bin :lib lib
    :src src :test test :conf conf
    :share share :doc doc
    :contrib contrib :public public
    :script script :cache cache :log log
    :template template :tmp tmp :opt opt
    :target target
    :readme-file readme-file
    :license-file license-file
    :install-file install-file))
  (jlog:info "Skelethon ``~a'' registered" name))

(defmacro defskelethon (name &body options)
  `(apply 'register-skelethon ,(tosymbol name) ',options))

(defgeneric initialize-license (skelethon)
  )

(defmethod initialize-license ((skelethon skelethon)) ;; TODO:
  t)

(defgeneric initialize-readme (skelethon)
  )

(defmethod initialize-readme ((skelethon skelethon)) ;; TODO:
  t)

(defgeneric initialize-install (skelethon)
  )

(defmethod initialize-install ((skelethon skelethon)) ;; TODO:
  t)

(defun make-skelethon-directory (dir &key prefix force) ;; FIXME: rewrite for correct pathnames
  (check-type dir skelethondir)
  (check-type prefix (or null string pathname))
  (when (or (and
             (listp dir)
             (or (eq (nth 1 dir) :required)
                 (eq (nth 2 dir) :required)))
            (not (null force)))
    (let* ((path-dir (pathname-as-directory (parse-namestring (car dir))))
           (path-prefix (if (stringp prefix) (pathname-as-directory (parse-namestring prefix)) prefix))
           (mode (when (integerp (nth 1 dir)) (nth 1 dir)))
           list
           (full-dir-path (merge-pathnames path-dir path-prefix)))
      ;;
      (push full-dir-path list)
      ;; add mode
      (when (not (null mode))
        (nconc list (list :mode mode)))
      (cond ((and
              (file-exists-p full-dir-path)
              (not (directory-pathname-p full-dir-path)))
             (jlog:err "Directory ``~a'' is regular file. Can not create directory" full-dir-path))
            ((directory-exists-p (merge-pathnames path-dir path-prefix))
             (jlog:dbg "Directory ``~a'' already exists. Skipping" full-dir-path))
            (t
             (jlog:info "Making directory ``~a''" full-dir-path)
             (apply 'ensure-directories-exist list))))))

(defgeneric initialize-skelethon (skelethon &key prefix force)
  )

(defmethod initialize-skelethon ((skelethon skelethon) &key (prefix "") (force nil))
  ;; directories-init
  (let ((dirs (list
               (get-skelethon-bin skelethon)
               (get-skelethon-lib skelethon)
               (get-skelethon-src skelethon)
               (get-skelethon-test skelethon)
               (get-skelethon-conf skelethon)
               (get-skelethon-share skelethon)
               (get-skelethon-doc skelethon)
               (get-skelethon-contrib skelethon)
               (get-skelethon-public skelethon)
               (get-skelethon-script skelethon)
               (get-skelethon-cache skelethon)
               (get-skelethon-log skelethon)
               (get-skelethon-template skelethon)
               (get-skelethon-tmp skelethon))))
    (jlog:dbg "Initializing skelethon ``~a'' in ``~a''"
              (get-skelethon-name skelethon)
              (merge-pathnames (pathname-as-directory (if (stringp prefix) (parse-namestring prefix) prefix))
                               (pathname-as-directory (os-pwd))))
    (dolist (dir (remove nil dirs))
      (make-skelethon-directory dir :prefix prefix :force force)))
  ;;
  (when (not (null (get-skelethon-opt skelethon)))
    (dolist (dir (get-skelethon-opt skelethon))
      (make-skelethon-directory dir :prefix prefix :force force)))
  ;; initializing standard files
  ;; (initialize-license skelethon)
  ;; (initialize-readme skelethon)
  ;; (initialize-install skelethon)
  (jlog:info ".[ Initialized skelethon ``~a'' in ``~a'' ]"
             (get-skelethon-name skelethon)
             (merge-pathnames (pathname-as-directory (if (stringp prefix) (parse-namestring prefix) prefix))
                              (pathname-as-directory (os-pwd))))
  )

;; (defskelethon default
;;   :bin "bin1"
;;   :lib ("lib1")
;;   :src ((:relative "src1") 0770 :required))

;; (initialize-skelethon (find-skelethon :test))

(defun parse-skelethon-from-file (file)
  (check-type file (or string pathname))
  (let* ((content (car (os-cat file :list)))
         (skelethon-name (tosymbol (car content)))
         (skelethon-body (cdr content)))
    ;;
    (jlog:dbg "Processing skelethon file ``~a''" file)
    (eval (cons 'defskelethon (cons skelethon-name skelethon-body)))))

(defun parse-skelethon-inline (name skelethon)
  (check-type name symbol)
  (check-type skelethon list)
  (eval (cons 'defskelethon (cons name skelethon))))

(defmacro make-get-project-smth-dir (dir) ;; TODO: rewrite w/o get-project-skelethon
  `(progn
     (defgeneric ,(intern (string-upcase (concatenate 'string "project-" dir "-dir"))) (project))
     (defmethod ,(intern (string-upcase (concatenate 'string "project-" dir "-dir"))) ((project project))
       (let ((dir (parse-namestring
                   (,(intern (string-upcase (concatenate 'string "get-skelethon-" dir)))
                     (find-skelethon
                      (or
                       (try (slot-value project 'skelethon))
                       (list *jabs-default-skelethon-name*))
                      ))))
             (project-pathname (get-project-pathname project))
             (workdir (os-pwd)))
         (cond ((eq :absolute (car (pathname-directory dir))) dir)
               ((eq :absolute (car (pathname-directory project-pathname)))
                (merge-pathnames dir project-pathname))
               (t (merge-n-directories workdir project-pathname dir)))))))

(dolist (d (list "src" "bin" "lib" "doc" "share" "test" "contrib" "conf" "public" "cache" "script" "log" "template" "opt" "tmp"))
  (eval `(make-get-project-smth-dir ,d)))

;; TODO: skelethon:create-from-project
;; TODO: show list of skelethons

;; TODO: +add project name to skelethon dirs
;; TODO: +add package naming convension
;; TODO: +add build.jab sample for all skelethons

(defun process-skelethon (skelethon-params)
  "process skelethon"
  (let* ((project-name (get-project-name *jabs-current-project*))
         (skelethon-name (if (atom skelethon-params) skelethon-params (car skelethon-params)))
         (skelethon-force (when (listp skelethon-params) (cadr skelethon-params)))
         (skelethon-obj (if skelethon-name
                            (find-skelethon skelethon-name)
                            (progn
                              (jlog:wrn "There is no skelethon ``~a'' for project ``~a''. Using default: ``~a''"
                                        skelethon-name project-name *jabs-default-skelethon-name*)
                              (find-skelethon *jabs-default-skelethon-name*)))))
    ;;
    (when (null skelethon-obj)
      (jlog:crit "There is no skelethon for project ``~a'' or default skeelethon ``~a''. Check your installation consistency"
                 project-name *jabs-default-skelethon-name*))
    (if skelethon-force
        (jlog:dbg "Processing skelethon ``~a'' for project ``~a'' FORCE" skelethon-name project-name)
        (jlog:dbg "Processing skelethon ``~a'' for project ``~a''" skelethon-name project-name))
    (setf skelethon-name (tosymbol (gensym "SKELETHON-")))
    (initialize-skelethon skelethon-obj :force skelethon-force)))

(defun find-skelethon-file (name)
  (check-type name (or string symbol))
  (let ((skelethon-name (string-downcase (princ-to-string name)))
        (skelethon-dirs))
    (dolist (dirs *jabs-skelethon-directories*)
      (dolist (dir (directory
                    (merge-pathnames
                     (make-pathname :name skelethon-name :type *jabs-skelethon-template-type*)
                     dirs)))
        (push dir skelethon-dirs)))
    (car skelethon-dirs)))

(defun load-skelethon (skelethon)
  (let ((skelethon-name (if (listp skelethon) (car skelethon) skelethon)))
    (when (and (not (find-skelethon skelethon-name)) (or (atom skelethon) (null (cdr skelethon)) (eq (cdr skelethon) :force)))
      (let ((file-to-load
	     (find-skelethon-file (string-downcase (princ-to-string skelethon-name)))))
	(if file-to-load
	    (parse-skelethon-from-file file-to-load)
	  (jlog:crit "There is no file for skelethon ~a" skelethon-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; extending defproject syntax
(bind-project-symbol
 :skelethon
 #'(lambda (x)
     (declare (ignore x))))

(defgeneric load-project-skelethon (project)
  )

(defmethod load-project-skelethon ((project project))
  (let ((skelethon
         (or
          (try (slot-value project 'skelethon))
          (list *jabs-default-skelethon-name*))))
    (load-skelethon skelethon)))

(defgeneric process-project-skelethon (project)
  )

(defmethod process-project-skelethon ((project project))
  (let ((skelethon (or
                    (try (slot-value project 'skelethon))
                    (list *jabs-default-skelethon-name*))))
    (process-skelethon skelethon)))

;;;; adding define-project-hook to load skelethon
(add-hook *define-project-hook* #'load-project-skelethon)

;;;; adding run-project-hook to process skelethon
;; (add-hook *run-project-hook* #'process-project-skelethon)
