(in-package #:tmpl@generic@plugin@jabs)

;; fix, if some project selected
(setf *jabs-projects-to-run* nil)

;; no plugins to run, so run one
(add-hook
    *post-init-hook*
  (lambda ()
    (if (file-exists-p (merge-pathnames (make-pathname :name "build" :type "jab") (os-pwd)))
        (jlog:crit "Buildfile already exists. Nothing to create")
        (progn
          (load-skelethon (list *tmpl-skelethon-name*))
          ;;
          (let ((project-name (car (reverse (pathname-directory (os-pwd))))))
            (run-project (find-project (tools@jabs:tostr project-name))))))))
