(in-package :quicklisp@repository@plugin@jabs)

;;; API

(defun find-repository-project (name)
  (funcall (tools@jabs:tosymbol :find-system :ql-dist) name))

(defun load-repository-project (name)
  (jlog:info "Loading package ``~a''" name)
  (funcall (tools@jabs:tosymbol :quickload :ql) name))

(defun remove-repository-project (name)
  (funcall (tools@jabs:tosymbol :uninstall :ql) name))

(defun repository-project-version (name)
  (slot-value
   (slot-value
    (find-repository-project name)
    (tools@jabs:tosymbol :release :ql-dist))
   (tools@jabs:tosymbol :prefix :ql-dist)))

(defun update-project-repository-projects ()
  (funcall (tools@jabs:tosymbol :update-all-dists :ql)))

(defun repository-project-dependencies (name)
  (let ((project (find-repository-project name)))
    (if project
        (mapcar #'tools@jabs:tosymbol (slot-value project (tools@jabs:tosymbol :required-systems :ql-dist)))
        (jlog:crit "There is no project, named ``~a'' in quickslisp. Can not resolve dependencies" name))))


;; ;; (defun artifact-installed-p (artifact source &key url)
;; ;;   (declare (ignore url source))
;; ;;   (jlog:dbg "HERE")
;; ;;   (let ((system (find-artifact artifact)))
;; ;;     (when (not (null system))
;; ;;       (funcall (intern "INSTALLEDP" :ql-dist) system))))

;; (defmethod install-artifact (name (repo :quicklisp-repository))
;;   (jlog:info "Installing package ~a" name)
;;   (funcall (tools@jabs:tosymbol :quickload :ql) name))

;; ;; (defun install-artifact (artifact source &key url)
;; ;;   (declare (ignore source))
;; ;;   (declare (ignore url)) ;; we don't use custom URL in quicklisp
;; ;;   (let ((system (find-artifact artifact)))
;; ;;     (when (not (null system))
;; ;;       (funcall (intern "INSTALL" :ql-dist) system))))

;; ;; (defun load-artifact (artifact source &key url)
;; ;;   (declare (ignore source))
;; ;;   (declare (ignore url)) ;; we don't use custom URL in quicklisp
;; ;;   (funcall (intern "QUICKLOAD" :ql) artifact))

;; ;; (defun uninstall-artifact (artifact source &key url)
;; ;;   (declare (ignore source))
;; ;;   (declare (ignore url)) ;; we don't use custom URL in quicklisp
;; ;;   (funcall (intern "UNINSTALL" :ql-dist) artifact))

;; ;; (defun update-artifact (artifact source &key url)
;; ;;   (install-artifact artifact source :url url))

;; ;; (defun artifact-status (artifact source &key url) ;; installed-p
;; ;;   (declare (ignore url))
;; ;;   (artifact-installed-p artifact source))

;; ;; (defun artifact-version (artifact source &key url)
;; ;;   (declare (ignore source))
;; ;;   (declare (ignore url))
;; ;;   (slot-value
;; ;;    (slot-value
;; ;;     (find-artifact artifact)
;; ;;     (intern "RELEASE" :ql-dist))
;; ;;    (intern "PREFIX" :ql-dist)))

;; ;; (defun list-artifacts ()
;; ;;   (mapcar #'(lambda (x) (slot-value x (intern "NAME" :ql-dist))) (funcall (intern "SYSTEM-LIST" :ql))))

;; ;; (defun refresh-repository ()
;; ;;   (initialize-repository))

;; ;; (defun get-dependency-tree (name)
;; ;;   (let ((dependencies))
;; ;;     (labels ((get-deps-tree (package-name)
;; ;;                             (pushnew package-name dependencies :test 'string-equal)
;; ;;                             (let ((deps (get-package-dependencies package-name)))
;; ;;                               (if (null deps)
;; ;;                                   (jlog:dbg "- Deps for ``~a'' is null" package-name)
;; ;;                                 (progn
;; ;;                                   (jlog:dbg "- Deps for ``~a'' is not null: ``~a''" package-name deps)
;; ;;                                   (dolist (v deps)
;; ;;                                     (pushnew v dependencies :test 'string-equal)
;; ;;                                     (get-deps-tree v)))
;; ;;                                 ))))
;; ;;       (get-deps-tree name)
;; ;;       dependencies)))

;; (defmethod artifact-dependencies (name (repo :quicklisp-repository))
;;   (slot-value
;;    (funcall (tools@jabs:tosymbol :find-system :ql-dist) name)
;;    (tools@jabs:tosymbol :required-systems :ql-dist)
;;    ))


;; ;; (defun artifact-dependencies (artifact &key (recursive t))
;; ;;   (if recursive
;; ;;       (funcall (intern "WHO-DEPENDS-ON" :ql) "cl-fad")
;; ;;     (slot-value (find-artifact artifact) (intern "REQUIRED-SYSTEMS" :ql-dist))))

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
