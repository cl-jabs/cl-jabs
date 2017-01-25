

(bind-project-symbol :formatting #'the-format)

(defun the-format (projectname &rest args)
  (declare (ignore args))
  (let ((project (find-project projectname)))
    (format t "~a~%" (class-of project))))
