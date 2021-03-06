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
(defpackage depmanager@core@plugin@jabs
  (:use :cl :jabs))

(in-package :depmanager@core@plugin@jabs)

(make-instance 'jabs::plugin :name :depmanager :type :core :version jabs::+jabs-version+)

(in-package :jabs)

(define-plugin-type :repository plugin
  (let ((checker t)
        (plugin-package-name
         (eval (append '(concat-keywords-w-delimiter *jabs-universal-delimiter*)
                       (list (get-plugin-name plugin))
                       (list (get-plugin-type plugin))
                       +jabs-plugin-namespace+))))
    ;; TODO: check for functions format (args and their types)
    (dolist (v '(:initialize-repository :find-repository-project
                 :load-repository-project :remove-repository-project
                 :repository-project-version :update-repository-project-list
                 :repository-project-dependencies))
      (when (not (try (symbol-function (tosymbol v plugin-package-name))))
        (setf checker nil)))
    checker))

(defvar *jabs-global-projects-map* (make-hash-table)
  "Define hash with project/file keys")

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

;; register global projects
(add-hook *post-init-hook* #'map-global-projects-to-files)

(defgeneric get-project-projects-map (project)
  )

(defmethod get-project-projects-map ((project project))
  "Get project names mapped to files for only for given project
w\o global map"
  (let* ((skel-name (get-project-skeleton-name project))
         (skel (or
                (find-skeleton skel-name)
                (and
                 (load-skeleton skel-name)
                 (find-skeleton skel-name))))
         (lib (or (try (car (get-skeleton-lib skel)))
                  (get-skeleton-lib skel)))
         (contrib (or (try (car (get-skeleton-contrib skel)))
                      (get-skeleton-contrib skel)))
         (opt (get-skeleton-opt skel))
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

(defgeneric get-project-depends-on (project)
  )

(defmethod get-project-depends-on ((project project))
  (try (slot-value project 'depends-on)))

(defgeneric dependency-project-reachable-locally-p (name project)
  )

(defmethod dependency-project-reachable-locally-p (name (project project))
  (check-type name keyword)
  (or (gethash name (get-project-projects-map project))
      (gethash name *jabs-global-projects-map*)))

(defgeneric find-project-dependency-force-locally (name project)
  )

(defmethod find-project-dependency-force-locally (name (project project))
  "Like ``find-project'', but tries to load local project file,
than retry to find project"
  (let* ((found-project (find-project name))
         (found-project-file-jab
          (when (null found-project)
            (dependency-project-reachable-locally-p name project))))
    (or found-project
        (progn
          (when found-project-file-jab
            (load found-project-file-jab))
          ;;
          (setf found-project (find-project name))
          ;;
          (if (and (null found-project) found-project-file-jab)
              (jlog:crit "Incorrect format of project file ``~a''"
                         found-project-file-jab)
              found-project)))))

(defgeneric dependency-project-reachable-remotely-p (name project)
  )

(defmethod dependency-project-reachable-remotely-p (name (project project))
  "Check if project can be reachable in all accessible plugins"
  (let ((plugins (filter-project-plugins-by-type project :repository)))
    (plugins-api-call-to-true
     :find-repository-project plugins name)))

(defgeneric find-project-dependencies-remotely (name project)
  )

(defmethod find-project-dependencies-remotely (name (project project))
  "Check if project can be reachable in all accessible plugins"
  (let ((plugins (filter-project-plugins-by-type project :repository)))
    (plugins-api-call-to-true
     :repository-project-dependencies plugins name)))

(defgeneric load-project-remote-dependency (name project)
  )

(defmethod load-project-remote-dependency (name (project project))
  "Check if project can be reachable in all accessible plugins"
  (let ((plugins (filter-project-plugins-by-type project :repository)))
    (plugins-api-call-to-true
     :load-repository-project plugins name)))

(defgeneric dependency-project-reachable-p (name project)
  )

(defmethod dependency-project-reachable-p (name (project project))
  (or (dependency-project-reachable-locally-p name project)
      (dependency-project-reachable-remotely-p name project)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric make-project-depencencies-list (project)
  )

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
                  (make-deps-list (tokeyword (car all-deps)) :other-deps (cdr all-deps) :collector (cons name collector)))))))
    (remove (get-project-name project)
            (reverse (remove-duplicates (reverse (make-deps-list (get-project-name project))))))))

(defgeneric load-project-local-dependency (name project)
  )

(defmethod load-project-local-dependency (name (project project))
  (when (find-project-dependency-force-locally name project)
    (jlog:dbg "Loading project ``~a'' as local dependency for project ``~a''" name (get-project-name project))
    (asdf:operate 'asdf:load-op name)))

(defgeneric load-project-dependencies (project)
  )

(defmethod load-project-dependencies ((project project))
  (dolist (v (make-project-depencencies-list project))
    (jlog:info "Loading dependency ``~a'' for project ``~a''"
               v (get-project-name project))
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
                `(concat-keywords-w-delimiter *jabs-universal-delimiter* ,name ,type)
                +jabs-plugin-namespace+)))
             (project (find-project plugin-package-name)))
        (jlog:dbg "Loading plugin ``~a'' dependencies"
                  (concat-keywords-w-delimiter *jabs-universal-delimiter* name type))
        (load-project-dependencies project))))
