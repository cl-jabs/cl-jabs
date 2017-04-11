(in-package :tgz@archive@plugin@jabs)

(defvar *archive-extension* "tgz")

;; (export '(:make-project-archive))

(defun make-project-archive (name version source)
  (let ((archive-name (concatenate 'string (tostr name t) "-" (tostr version t) "." *archive-extension*))
        (filelist))
    ;; change directory to source and find files with paths, related to source dir
    (os-cd source)
    (setf filelist (os-find "." :type :file))

    ;; (defun create-tar-file (pathname filelist)
    (archive:with-open-archive (archive archive-name :direction :output
                                        :if-exists :supersede)
      (dolist (file filelist (archive:finalize-archive archive))
        (let ((entry (archive:create-entry-from-pathname archive file)))
          (archive:write-entry-to-archive archive entry))))))
