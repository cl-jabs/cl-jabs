(defpackage depmanager@core@plugin@jabs
  (:use :cl :jabs))

(in-package :depmanager@core@plugin@jabs)

(make-instance 'jabs::plugin :name :depmanager :type :core :version jabs::+jabs-version+)

(in-package :jabs)

(define-plugin-type :repository plugin
  (let ((checker t)
        (plugin-package-name
         (eval (append '(concatenate-symbol *jabs-universal-delimiter*)
                       (list (get-plugin-name plugin))
                       (list (get-plugin-type plugin))
                       +jabs-plugin-namespace+))))
    ;; TODO: check for functions format (args and their types)
    (dolist (v '(:initialize-repository :find-repository-project
                 :load-repository-project :remove-repository-project
                 :repository-project-version :update-project-repository-projects
                 :repository-project-dependencies))
      (when (not (try (symbol-function (tosymbol v plugin-package-name))))
        (setf checker nil)))
    checker))

(defvar *jabs-global-projects-map* (make-hash-table)
  "Define hash with project/file keys")

(defvar *jabs-global-systems-map* (make-hash-table)
  "Define hash with system/file keys. For ASDF compatability")

(defun map-global-projects-to-files ()
  "Register map with projects from global paths"
  (let ((sources
         (append
          (os-find
           (merge-pathnames (make-pathname :directory '(:relative "lib"))
                            *jabs-lib-directory*)
           :name "build" :extension "jab"))))
    (dolist (file sources)
      (dolist (l (os-cat file :list))
        (when (eq (tosymbol (car l)) :defproject)
          (if (atom (cadr l))
              (progn
                (jlog:dbg "Mapping project ``~a'' to file ``~a''" (tosymbol (cadr l)) file)
                (setf (gethash (tosymbol (cadr l)) *jabs-global-projects-map*) file))
              (jlog:wrn "Incorrect project format in file ``~a''. Skipping" file)))))))

(defun map-global-systems-to-files ()
  "Register map with systems from global paths
Created for ASDF compatability"
  (let ((sources
         (append
          (os-find
           (merge-pathnames (make-pathname :directory '(:relative "lib"))
                            *jabs-lib-directory*)
           :extension "asd"))))
    (dolist (file sources)
      (dolist (l (os-cat file :list))
        (when (eq (tosymbol (car l)) :defsystem)
          (if (atom (cadr l))
              (progn
                (jlog:dbg "Mapping asdf system ``~a'' to file ``~a''" (tosymbol (cadr l)) file)
                (setf (gethash (tosymbol (cadr l)) *jabs-global-systems-map*) file))
              (jlog:wrn "Incorrect system format in file ``~a''. Skipping" file)))))))

;; register global projects and systems
(add-hook *post-init-hook* #'map-global-projects-to-files)
(add-hook *post-init-hook* #'map-global-systems-to-files)

(defmethod get-project-projects-map ((project project))
  "Get project names mapped to files for only for given project
w\o global map"
  (let* ((skel-name (or (car (try (slot-value project 'skelethon)))
                        *jabs-default-skelethon-name*))
         (skel (or
                (find-skelethon skel-name)
                (and
                 (load-skelethon skel-name)
                 (find-skelethon skel-name))))
         (lib (or (try (car (get-skelethon-lib skel)))
                  (get-skelethon-lib skel)))
         (contrib (or (try (car (get-skelethon-contrib skel)))
                      (get-skelethon-contrib skel)))
         (opt (get-skelethon-opt skel))
         (hash (make-hash-table)))
    ;;
    (mapcar #'(lambda (x)
                (dolist (f
                          (os-find (merge-pathnames (parse-namestring x) +jabs-run-directory+)
                                   :name "build" :extension "jab"))
                  (dolist (v (os-cat f :list))
                    (when (eq (tosymbol (car v)) :defproject)
                      (setf (gethash (tosymbol (cadr v)) hash) f)))))
            (cons lib (cons contrib opt)))
    hash))

(defmethod get-project-systems-map ((project project))
  "Get asdf system names mapped to files for only for given project
w\o global map. For ASDF back compatability"
  (let* ((skel-name (or (car (try (slot-value project 'skelethon)))
                        *jabs-default-skelethon-name*))
         (skel (or
                (find-skelethon skel-name)
                (and
                 (load-skelethon skel-name)
                 (find-skelethon skel-name))))
         (lib (or (try (car (get-skelethon-lib skel)))
                  (get-skelethon-lib skel)))
         (contrib (or (try (car (get-skelethon-contrib skel)))
                      (get-skelethon-contrib skel)))
         (opt (get-skelethon-opt skel))
         (hash (make-hash-table)))
    ;;
    (mapcar #'(lambda (x)
                (dolist (f
                          (os-find (merge-pathnames (parse-namestring x) +jabs-run-directory+)
                                   :extension "asd"))
                  (dolist (v (os-cat f :list))
                    (when (eq (tosymbol (car v)) :defsystem)
                      (setf (gethash (tosymbol (cadr v)) hash) f)))))
            (cons lib (cons contrib opt)))
    hash))

(defmethod get-project-depends-on ((project project))
  (try (slot-value project 'depends-on)))

(defmethod dependency-project-reachable-locally-p (name (project project))
  (check-type name keyword)
  (or (gethash name (get-project-projects-map project))
      (gethash name *jabs-global-projects-map*)))

(defmethod dependency-system-reachable-locally-p (name (project project))
  (check-type name keyword)
  (or (gethash name (get-project-systems-map project))
      (gethash name *jabs-global-systems-map*)))

(defmethod find-project-dependency-force-locally (name (project project))
  "Like ``find-project'', but tries to load local project or
system file, than retry to find project"
  (let* ((found-project (find-project name))
         (found-project-file-jab
          (when (null found-project)
            (dependency-project-reachable-locally-p name project)))
         (found-project-file-asd
          (when (null found-project)
            (dependency-system-reachable-locally-p name project))))
    (or found-project
        (progn
          (cond (found-project-file-asd
                 ;; we need to load ASDF systems inside of :asdf package
                 (let ((*package* (find-package :asdf)))
                   (load found-project-file-asd)))
                (found-project-file-jab
                 (load found-project-file-jab)))
          ;;
          (setf found-project (find-project name))
          ;;
          (if (and (null found-project)
                   (or
                    found-project-file-jab
                    found-project-file-asd))
              (jlog:crit "Incorrect format of project file ``~a''"
                         (or
                          found-project-file-jab
                          found-project-file-asd))
              found-project)))))

(defmethod dependency-project-reachable-remotely-p (name (project project))
  "Check if project can be reachable in all accessible plugins"
  (let ((plugins (filter-project-plugins-by-type project :repository)))
    (plugins-api-call-to-true
     :find-repository-project plugins name)))

(defmethod find-project-dependencies-remotely (name (project project))
  "Check if project can be reachable in all accessible plugins"
  (let ((plugins (filter-project-plugins-by-type project :repository)))
    (plugins-api-call-to-true
     :repository-project-dependencies plugins name)))

(defmethod load-project-remote-dependency (name (project project))
  "Check if project can be reachable in all accessible plugins"
  (let ((plugins (filter-project-plugins-by-type project :repository)))
    (plugins-api-call-to-true
     :load-repository-project plugins name)))

(defmethod dependency-project-reachable-p (name (project project))
  (or (dependency-project-reachable-locally-p name project)
      (dependency-system-reachable-locally-p name project)
      (dependency-project-reachable-remotely-p name project)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-project-depencencies-list ((project project))
  "Make full list of project dependencies (recursively)"
  (labels
      ((make-deps-list (name &key other-deps collector)
         (let* ((me (find-project-dependency-force-locally name project))
                (me-deps ;; FIXME: do not load local project to get deps.
                 (if me  ;; Just use os-cat
                     (get-project-depends-on me)
                     (if (dependency-project-reachable-remotely-p name project)
                         (find-project-dependencies-remotely name project)
                         (jlog:crit "Can not reach dependency ``~a'' for project ``~a'' in any way. Can not resolve dependencies"
                                    name (get-project-name project)))))
                (all-deps (append other-deps me-deps)))
           ;;
           (cond ((null all-deps)
                  (cons name collector))
                 (t
                  (make-deps-list (car all-deps) :other-deps (cdr all-deps) :collector (cons name collector)))))))
    (remove (get-project-name project)
            (reverse (remove-duplicates (reverse (make-deps-list (get-project-name project))))))))

(defmethod load-project-local-dependency (name (project project))
  (jlog:dbg "Loading project ``~a'' as dependency for project ``~a''" name (get-project-name project))
  (and (find-project-dependency-force-locally name project)
       (asdf:operate 'asdf:load-op name)))

(defmethod load-project-dependencies ((project project))
  (dolist (v (make-project-depencencies-list project))
    (or
     (load-project-local-dependency (tosymbol v) project)
     (load-project-remote-dependency (tosymbol v) project)
     (jlog:crit "Can not load dependency ``~a'' for project ``~a'' in any way"
                v (get-project-name project)))))

(add-hook *pre-load-plugin-hook*
  #'(lambda (x)
      ;; we need special behavior for plugins.
      ;; Plugin requires to load it's dependencies
      ;; before plugin loading.
      (let* ((name (get-plugin-name x))
             (type (get-plugin-type x))
             (plugin-package-name
              (eval
               (append
                `(concatenate-symbol *jabs-universal-delimiter* ,name ,type)
                +jabs-plugin-namespace+)))
             (project (find-project plugin-package-name)))
        (jlog:dbg "Loading plugin ``~a'' dependencies"
                  (concat-keywords-w-delimiter *jabs-universal-delimiter* name type))
        (load-project-dependencies project))))

;; find-project
;; project-installed-p
;; project-loaded-p
;; project-status
;; project-version
;; install-project
;; update-project
;; project-dependencies

;; process depends-on
;; connect to asdf
