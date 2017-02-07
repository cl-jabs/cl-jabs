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
(in-package :quicklisp@repository@plugin@jabs)

(defun initialize-repository (path)
  ;; (jlog:note "Initializing repository plugin in ~a" path)
  ;; )
  (jlog:dbg "Initializing repository plugin ``quicklisp@repository'' in ``~a''" path)
  (let ((proj *jabs-current-project*)
        (quicklisp-setup-file (merge-pathnames (make-pathname :name "setup" :type "lisp") path)))
    (jlog:info "Initializing quicklisp repository for project ``~a''" (get-project-name proj) path)
    (jlog:dbg "Repository path is ``~a''" path)
    (or
     (progn
       (jlog:dbg "Trying to load quicklisp")
       (when (file-exists-p quicklisp-setup-file)
         (load quicklisp-setup-file)))
     (progn
       (jlog:info "It seems like quicklisp is not installed. Trying to install quicklisp")
       (funcall (eval `(function ,(tools@jabs:tosymbol :install :quicklisp-quickstart))) :path path)
       (jlog:dbg "Quicklisp installed")))))

(with-project-to-be-run
 *jabs-current-project*
 (let* ((skeleton (find-project-skeleton *jabs-current-project*))
	(skeleton-cache-dir
	 (or (try (pathname-as-directory (parse-namestring (car (get-skeleton-cache skeleton)))))
	     (try (pathname-as-directory (parse-namestring (get-skeleton-cache skeleton))))
	     (make-pathname :directory '(:absolute "tmp"))))
	(path (merge-pathnames
	       (merge-pathnames
		(make-pathname :directory
			       (list :relative "quicklisp"
				     (string-downcase (princ-to-string (get-project-name *jabs-current-project*)))))
		skeleton-cache-dir)
	       jabs::+jabs-run-directory+)))
   (initialize-repository path)
   (share-plugin (find-plugin :quicklisp :repository))
   ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun find-repository-project (name)
;;   (funcall (tools@jabs:tosymbol :find-system :ql-dist) name))

;; (defun load-repository-project (name)
;;   (jlog:info "Loading package ``~a''" name)
;;   (funcall (tools@jabs:tosymbol :quickload :ql) name))

;; (defun remove-repository-project (name)
;;   (funcall (tools@jabs:tosymbol :uninstall :ql) name))

;; (defun repository-project-version (name)
;;   (slot-value
;;    (slot-value
;;     (find-repository-project name)
;;     (tools@jabs:tosymbol :release :ql-dist))
;;    (tools@jabs:tosymbol :prefix :ql-dist)))

;; (defun update-project-repository-projects ()
;;   (funcall (tools@jabs:tosymbol :update-all-dists :ql)))

;; (defun repository-project-dependencies (name)
;;   (let ((project (find-repository-project name)))
;;     (if project
;;         (mapcar #'tools@jabs:tosymbol (slot-value project (tools@jabs:tosymbol :required-systems :ql-dist)))
;;         (jlog:crit "There is no project, named ``~a'' in quickslisp. Can not resolve dependencies" name))))

;; ;; (defun load-artifact (artifact source &key url)
;; ;;   (declare (ignore source))
;; ;;   (declare (ignore url)) ;; we don't use custom URL in quicklisp
;; ;;   (funcall (intern "QUICKLOAD" :ql) artifact))

;; ;; (defun update-artifact (artifact source &key url)
;; ;;   (install-artifact artifact source :url url))

;; ;; (defun refresh-repository ()
;; ;;   (initialize-repository))

;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; ;; hook for adding additional sources to quicklisp registry
;; ;; ;; (add-hook *register-source-hook*
;; ;; ;;           #'(lambda (x)
;; ;; ;;               (eval `(push ,x ,(intern "*LOCAL-PROJECT-DIRECTORIES*" :ql)))))

;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; ;;   (setf (fdefinition  'dist) (fdefinition (intern "DIST" "QL-DIST")))
;; ;; ;;   (setf (fdefinition  'available-versions) (fdefinition (intern "AVAILABLE-VERSIONS" "QL-DIST")))
;; ;; ;;   (setf (fdefinition  'enabled-dists) (fdefinition (intern "ENABLED-DISTS" "QL-DIST")))
;; ;; ;;   (setf (fdefinition  'version) (fdefinition (intern "VERSION" "QL-DIST")))
;; ;; ;;   (setf (fdefinition  'install-dist) (fdefinition (intern "INSTALL-DIST" "QL-DIST")))

;; ;; ;;   (flet ((available-version-p (version)
;; ;; ;;            (find version (mapcar #'version (enabled-dists)) :test #'string=)))

;; ;; ;;     (let* ((available-versions (available-versions (dist "quicklisp")))
;; ;; ;;            (version-distinfo-url))

;; ;; ;;       (when (eq version :latest)
;; ;; ;;         (setf version (caar available-versions)))

;; ;; ;;       (setf version-distinfo-url (cdr (assoc version available-versions :test #'string=)))

;; ;; ;;       (unless (available-version-p version)
;; ;; ;;         (format t "Installing dist version ~A~%" version)
;; ;; ;;         (install-dist version-distinfo-url :replace t :prompt nil))
;; ;; ;;       )))

;; ;; ;; (declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))

;; ;; ;; (let ((version-file (parse-namestring ".quicklisp-version")))
;; ;; ;;   (when (probe-file version-file)
;; ;; ;;     (require-quicklisp
;; ;; ;;      :version (with-open-file
;; ;; ;;                   (in version-file :direction :input)
;; ;; ;;                 (read in)))))
;; ;; ;;  )



;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; ;; #+sbcl(require 'sb-posix)
;; ;; ;; #+sbcl(setf sb-impl::*default-external-format* :utf-8)

;; ;; ;; (ql:quickload :alexandria)
;; ;; ;; (ql:quickload :cl-ppcre)
;; ;; ;; (ql:quickload :iolib)
;; ;; ;; (ql:quickload :cl-fad)
;; ;; ;; (ql:quickload :clsql)
;; ;; ;; https://gist.github.com/shortsightedsid/71cf34282dfae0dd2528
;; ;; ;; http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp
;; ;; ;; (ql:quickload :usocket)
;; ;; ;; (ql:quickload :cl-ghostcode)
;; ;; ;; (ql:quickload :named-hash-tables)
;; ;; ;; (ql:quickload :cl-actors)

;; ;; ;; (ql:quickload :cl-phinan)

;; ;; ;; (defvar *project-root* (pathname (nth 0 (argv))))
;; ;; ;; (defvar *port* (parse-integer (nth 1 (argv))))



;; ;; ;; (if (member "--no-linedit" (argv) :test 'equal)
;; ;; ;;     (setf sb-ext:*posix-argv* (remove "--no-linedit" sb-ext:*posix-argv* :test 'equal))
;; ;; ;;     (when (interactive-stream-p *terminal-io*)
;; ;; ;;       (require :sb-aclrepl)
;; ;; ;;       (ql:quickload :linedit)
;; ;; ;;       (funcall (intern "INSTALL-REPL" :linedit) :wrap-current t)))

;; ;; ;; (ql:quickload :cl-ppcre)

;; ;; ;; sending "\\033[2J" to clear screen
;; ;; ;; (format t "~C[2J~%" #\Esc)

;; (format t "Wellcome to PHINAN. It`s very future technology. Let`s hack, baby~%")
