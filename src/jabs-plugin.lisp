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
(in-package :jabs)

(export '(defplugin
          find-plugin
          find-plugin-type
          register-plugin-type
          get-plugin-name
          get-plugin-type
          get-plugin-url
          get-plugin-path
          get-plugin-version))

(defconstant +jabs-plugin-namespace+ '(:plugin :jabs))

(defvar *jabs-plugin-registry* (make-hash-table))
(defvar *jabs-plugin-type-registry* (make-hash-table))

(defvar *jabs-plugin-directories*
  (list
   *jabs-source-directory*
   (merge-pathnames (make-pathname :directory '(:relative "plugins")) *jabs-share-directory*)
   (merge-pathnames (make-pathname :directory '(:relative ".jabs" "plugins")) (user-homedir-pathname))
   (merge-pathnames (make-pathname :directory '(:relative ".jabs" "plugins")) +jabs-run-directory+)))

;;;; full plugin names: name@type@plugin@jabs
;; plugin types: generic, core, repository, test etc

(defvar *pre-define-plugin-hook* nil)
(defvar *define-plugin-hook* nil)

(defvar *pre-load-plugin-hook* nil)
(defvar *load-plugin-hook* nil)

(defvar *jabs-shared-plugin-names* nil
  "Plugin names, which should be shared between all projects.
Needed for plugins, like repository plugins to get dependencies
by many projects w/o requirement to set it as plugin in
 every project etc")

(defvar *jabs-cli-plugin-names* nil)
;;;;
;;;; plugin types
;;;;
(defun find-plugin-type (name)
  "Check if plugin type already registered"
  (check-type name keyword)
  (gethash name *jabs-plugin-type-registry*))

(defun register-plugin-type (name &optional (validator #'(lambda (x) (declare (ignore x)) t)))
  "Register plugin type. Skip, when already registered"
  (check-type name keyword)
  (check-type validator function)
  (if (find-plugin-type name)
      (jlog:dbg "Plugin type ``~a'' alrealy registered. Skipping" name)
      (progn
        (jlog:dbg "Registering plugin type ``~a''" name)
        (setf (gethash name *jabs-plugin-type-registry*) validator)
        (if (find-plugin-type name)
            (progn (jlog:info "Plugin type ``~a'' registered" name) t)
            (jlog:err "Can not register plugin type ``~a'' for unknown reason" name)))))

(defmacro define-plugin-type (name arg &body validator)
  `(register-plugin-type ,(tokeyword name)
                         (lambda (,arg)
                           (check-type ,arg plugin)
                           ,@validator)))

;;;;
;;;; plugins
;;;;
(defclass plugin ()
  ((name :accessor get-plugin-name :initarg :name)
   (type :accessor get-plugin-type :initarg :type)
   (uri :accessor get-plugin-uri :initarg :uri)
   (path :accessor get-plugin-path :initarg :path)
   (version :accessor get-plugin-version :initarg :version)))

(defun make-plugin-package-name (name &optional (type :generic))
  "Make correct plugin name in jabs plugin namespace"
  (check-type name keyword)
  (check-type type keyword)
  (if (and
       (find-plugin-type type)
       (eval (append '(namespace-subtree-p) (reverse +jabs-plugin-namespace+))))
      (let ((namespace-list (append (list *jabs-universal-delimiter* name type) +jabs-plugin-namespace+)))
        (eval (cons 'concat-keywords-w-delimiter namespace-list)))
      (jlog:err "Incorrect plugin type ``~a''. Can not create plugin package name with name ``~a'' and type ``~a''" type name type)))

(defun find-plugin (name &optional (type :generic))
  "Find plugin, if it already registered"
  (check-type name keyword)
  (check-type type keyword)
  (let ((plugin-name (make-plugin-package-name name type)))
    (gethash plugin-name *jabs-plugin-registry*)))

(defun find-plugin-file (name &key (type :generic) (filename "build") (filetype "jab"))
  (check-type name (or string symbol))
  (check-type type (or string symbol))
  (check-type filename string)
  (check-type filetype string)
  (let ((plugin-name (string-downcase (string name)))
        (plugin-type (string-downcase (string type)))
        (plugin-dirs))
    (dolist (dirs *jabs-plugin-directories*)
      (dolist (dir (directory
                    (merge-pathnames
                     (make-pathname :directory (list :relative plugin-type plugin-name) :name filename :type filetype)
                     dirs)))
        (push dir plugin-dirs)))
    (car plugin-dirs)))

(defgeneric register-plugin (plugin)
  )

(defmethod register-plugin ((plugin plugin))
  "Add plugin to *jabs-plugin-registry*"
  (let* ((plugin-name (get-plugin-name plugin))
         (plugin-type (get-plugin-type plugin))
         (plugin-id (make-plugin-package-name plugin-name plugin-type)))
    (cond ((not (find-plugin-type plugin-type))
           (jlog:crit "Plugin type ``~a'' is not registered. Can not register plugin ``~a''"
                      plugin-type plugin-name))
          ((not plugin-id)
           (jlog:crit "Can not create full plugin name for ``~a'', type ``~a''. Can not register plugin"
                      plugin-name plugin-type))
          ((not (find-package plugin-id))
           (jlog:crit "Package ``~a'' is not found. Can not register plugin ``~a'', type ``~a''. Can not register plugin"
                      plugin-id plugin-name plugin-type))
          (t
           (jlog:dbg "Checking if plugin ``~a'' already registered" plugin-id)
           (if (gethash plugin-id *jabs-plugin-registry*)
               (jlog:wrn "Plugin ``~a'', type ``~a'' already registered. Skipping" plugin-name plugin-type)
               (progn
                 (jlog:dbg "Registering plugin ``~a''" plugin-id)
                 (setf (gethash plugin-id *jabs-plugin-registry*) plugin)
                 (jlog:info "Plugin ``~a'', type ``~a'' registered" plugin-id (get-plugin-type plugin))
                 t))))))

(defgeneric share-plugin (plugin)
  )

(defmethod share-plugin ((plugin plugin))
  "Add plugin to shared plugins"
  (push (concat-keywords-w-delimiter *jabs-universal-delimiter*
                            (get-plugin-name plugin)
                            (get-plugin-type plugin))
        *jabs-shared-plugin-names*))

(defmethod initialize-instance :after ((plugin plugin) &key)
  "Register plugin directly after initialization"
  (run-hook *pre-define-plugin-hook* plugin)
  (register-plugin plugin))

(defgeneric plugin-registered-p (plugin)
  )

(defmethod plugin-registered-p ((plugin plugin))
  "Checking if plugin already registered"
  (let ((name (make-plugin-package-name (get-plugin-name plugin) (get-plugin-type plugin))))
    (jlog:dbg "Checking, if plugin ``~a'', type ``~a'' registered"
              (get-plugin-name plugin)
              (get-plugin-type plugin))
    (gethash name *jabs-plugin-registry*)))

(defun load-plugin-file (name &key (type :generic) (filename "build") (filetype "jab"))
  (let ((plugin-file (find-plugin-file name :type type :filename filename :filetype filetype)))
    (if plugin-file
        (progn
          (jlog:dbg "Loading plugin file ``~a'' for ``~a'', type ``~a''"
                    plugin-file name type)
          (load (find-plugin-file name :type type :filename filename :filetype filetype)))
        (jlog:crit "Can not find plugin file ``~a'' for ``~a'', type ``~a''"
                   plugin-file name type))))

(defmacro defplugin (name type version &body body)
  "Define plugin"
  `(let ((plugin-id (make-plugin-package-name ,name ,type))
         (plugin-version ,version)
         (plugin-body ',body))
     ;;
     (if plugin-id
         (progn
           (when (not (find-package plugin-id))
             (eval
              `(defpackage ,plugin-id
                 (:use :cl :jabs :tools@jabs)))) ;; TODO: make something with this shit
           ;; Making and registering plugin (see initialize-instance)
           (let ((plugin-instance
                  (make-instance 'plugin :name ,name :type ,type :version ,version)))
             ;; Defining plugin project
             (eval
              `(defproject ,plugin-id
                   :name ,(string-downcase (princ-to-string plugin-id))
                   :version ,plugin-version
                   ,@plugin-body))
             (run-hook *define-plugin-hook* plugin-instance)
             ))
         (jlog:crit "Plugin type ``~a'' is not registered. Can not register plugin ``~a''" ,type ,name))))

(defun insert-plugin-dir (path)
  (check-type path pathname)
  (if (directory-exists-p path)
      (let ((plugdirs (reverse *jabs-plugin-directories*)))
        (and (push path plugdirs) (setf *jabs-plugin-directories* (reverse plugdirs))))
      (jlog:wrn "Plugin directory ``~a'' does not exist. Skipping" path)))

(defun add-plugin-dir (path)
  (check-type path pathname)
  (if (directory-exists-p path)
      (push path *jabs-plugin-directories*)
      (jlog:err "Plugin directory ``~a'' does not exist" path)))

(defun load-plugin (name &optional (type :generic))
  (check-type name symbol)
  (check-type type symbol)
  (let* ((plugin-name (string-downcase (princ-to-string name)))
         (plugin-type (string-downcase (princ-to-string type)))
         (plugin-instance (or
                           (find-plugin name type)
                           (and
                            (load-plugin-file plugin-name :type plugin-type)
                            (find-plugin name type))))
         (plugin-package-name
          (eval
           (append
            `(concat-keywords-w-delimiter *jabs-universal-delimiter* ,name ,type)
            +jabs-plugin-namespace+))))
    ;; perform load operations
    (if plugin-instance
        (progn
          (run-hook *pre-load-plugin-hook* plugin-instance)
          (require plugin-package-name))
        (jlog:crit "Can not load plugin ``~a'', type ``~a''. Plugin file not found" name type))
    ;; perform post-load operations
    (if (and (find-package plugin-package-name)
             (find-plugin name type)
             (let ((validator (gethash type *jabs-plugin-type-registry*))
                   (plugin (find-plugin name type)))
               (when (and plugin validator) ; validator must! be
                 (jlog:dbg "Validating plugin ``~a'', type ``~a''" name type)
                 (funcall validator plugin))))
        (progn (run-hook *load-plugin-hook* plugin-instance) plugin-instance)
        (jlog:crit "Can not load plugin ``~a'', type ``~a''. Format incorrect" name type))
    (jlog:info "Plugin ``~a'', type ``~a'' loaded" name type)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LIB
(defgeneric filter-project-plugins-by-type (project type)
  )

(defmethod filter-project-plugins-by-type ((project project) type)
  "Get plugin names list of same type plugins from project and shared plugins"
  (check-type type keyword)
  (let ((plugins (reverse
                  (remove-duplicates
                   (reverse
                    (append
                     (project-slot-value project 'plugins)
                     *jabs-shared-plugin-names*))))))
    (jlog:dbg "Found plugins ``~a'', type ``~a'' for project ``~a''" plugins type (get-project-name project))
    (remove-if
     #'(lambda (x) (null x))
     (mapcar #'(lambda (x)
                 (when (scan (tostr type)
                             (string-upcase (princ-to-string x))) x))
             plugins))))

(defmacro plugin-api-call (function-name plugin-name &body body)
  "Make API call to plugin repository"
  `(let ((plugin-package-name
          (eval (append '(concat-keywords-w-delimiter *jabs-universal-delimiter*)
                        (mapcar #'(lambda (x) (tokeyword x))
                                (split (car (tolist *jabs-universal-delimiter*))
                                       (princ-to-string ,plugin-name)))
                        +jabs-plugin-namespace+))))
     (jlog:dbg "Calling function ``~a'' from plugin ``~a''" ,function-name ,plugin-name)
     (funcall (tosymbol ,function-name plugin-package-name) ,@body)))

(defmacro plugins-api-call-to-true (function plugin-names &body body)
  "Makes calls in specific format: plugin-package::function <body>
 through list of corresponding plugins before true result"
  `(labels ((call-before-true (function plugins)
              (if (null plugins) nil
                  (let ((result
                         (plugin-api-call function (car plugins) ,@body)))
                    (or
                     result
                     (call-before-true function (cdr plugins)))))))
     (call-before-true ,function ,plugin-names)))

(defmacro plugins-api-call-all (function plugin-names &body body)
  "Makes calls in specific format: plugin-package::function <body>
 through list of all corresponding plugins"
  `(labels ((call-all (function plugins)
              (if (null plugins) nil
                  (progn
                    (plugin-api-call function (car plugins) ,@body)
                    (call-all function (cdr plugins))))))
	   (call-all ,function ,plugin-names)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; /LIB
;; Register plugin types
(define-plugin-type :generic plugin t)
(define-plugin-type :core plugin t)

;; bind symbol plugins
(bind-project-symbol
 :plugins
 #'(lambda (x)
     (declare (ignore x))))

;; bind CLI param to set project force
(bind-jabs-cli-parameter
 "plugins"
 #'(lambda (&rest x)
     (dolist (plugin x)
       (push (tokeyword plugin) *jabs-cli-plugin-names*))))

(bind-jabs-cli-parameter
 "system-plugins"
 #'(lambda (&rest x)
     (dolist (plugin x)
       (let ((plugin-name (tosymbol (car (split
                                          (car (tolist *jabs-universal-delimiter*))
                                          (princ-to-string plugin)))))
             (plugin-type (tosymbol (cadr (split
                                           (car (tolist *jabs-universal-delimiter*))
                                           (princ-to-string plugin))))))
         (load-plugin plugin-name plugin-type)))))

;; load plugin(s) defined in command line interface
(process-jabs-cli-parameter "plugins")

;; adding run-project-hook to load and process skeleton
(add-hook
    *define-project-hook*
  #'(lambda (x)
      (let ((*jabs-current-project* x) ;; TODO: is it needed?
            (plugins (if (eq (get-project-name *jabs-current-project*) *jabs-project-to-run*)
                         (append (project-slot-value x 'plugins) *jabs-cli-plugin-names*)
                         (project-slot-value x 'plugins)))
            (delimiter-symbol (car (tolist *jabs-universal-delimiter*))))
        ;;
        (dolist (plugin plugins)
          (let ((plugin-name (tosymbol (car (split delimiter-symbol (princ-to-string plugin)))))
                (plugin-type (tosymbol (cadr (split delimiter-symbol (princ-to-string plugin))))))
            (load-plugin plugin-name plugin-type))))))

;; TODO: realize plugin-type-validator function
