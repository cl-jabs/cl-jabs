(in-package :tgz@package@plugin@jabs)

(defvar *package-extension* "tgz")

(defun make-package (name version source)
  (let ((package-name (concatenate 'string (tostr name t) "-" (tostr version t) "." *package-extension*))
        (filelist))
    ;; change directory to source and find files with paths, related to source dir
    (os-cd source)
    (setf filelist (os-find "." :type :file))

    ;; (defun create-tar-file (pathname filelist)
    (archive:with-open-archive (archive package-name :direction :output
                                        :if-exists :supersede)
      (dolist (file filelist (archive:finalize-archive archive))
        (let ((entry (archive:create-entry-from-pathname archive file)))
          (archive:write-entry-to-archive archive entry))))))
