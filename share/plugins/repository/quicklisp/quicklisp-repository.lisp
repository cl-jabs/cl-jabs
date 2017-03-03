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
        (quicklisp-setup-file (merge-pathnames (make-pathname :name "setup" :type "lisp") path))
	(quicklisp-quickstart:*after-load-message* "")
	(quicklisp-quickstart:*after-initial-setup-message* "")
	)
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
