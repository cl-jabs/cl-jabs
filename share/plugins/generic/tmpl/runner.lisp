(in-package #:tmpl@generic@plugin@jabs)

;; fix, if some project selected
(setf *jabs-project-to-run* nil)

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
	    (setf *jabs-project-to-run* (tokeyword project-name))
            (run-project (find-project (tokeyword project-name))))))))
