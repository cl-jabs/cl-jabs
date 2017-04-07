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
#+sbcl(require :sb-posix)

(defpackage :logger@jabs
  (:nicknames :jlog)
  (:use :cl)
  ;; (:shadow :warn :error)
  (:export :crit
           :err
           :wrn
           :info
           :note
           :dbg
           :log-write-output
           :*log-level*
           :*log-string*
           :*fail-on-critical*
           :*fail-on-error*
           :*log-disable-notes*
           :get-log-level
           :set-log-level
           :define-output-type
           :enable-output-type
           :disable-output-type
           :make-log-string
           :get-stamp))

(defpackage re@jabs
  (:nicknames :jre)
  (:use :cl)
  (:export :get-before
           :get-after
           :get-first-n
           :cut-first-n
           :split
           :replace-string
           :replace-string-all
           :replace-inside-string
           :begin-scan
           :end-scan
           :middle-scan
           :end-cut
           :begin-cut
           :scan
           ;;
           :concatenate-to-string-with-delimiter))

(defpackage :tools@jabs
  (:use :cl :logger@jabs :jre)
  (:export :argv
           :terminate
           :maphash-keys
           :maphash-values
           :copy-hash-table
           :if-let
           :component-present-p
           :directory-pathname-p
           :pathname-as-directory
           :pathname-as-file
           :directory-wildcard
           :list-directory
           :file-exists-p
           :directory-exists-p
           :copy-file
           :lisp-version-string
           ;;
	   :os-hostname
	   :os-cd
	   :os-chdir
	   :os-dirname
	   :os-stat
	   :os-stat-atime
	   :os-stat-ctime
	   :os-stat-dev
	   :os-stat-gid
	   :os-stat-ino
	   :os-stat-mode
	   :os-stat-mtime
	   :os-stat-nlink
	   :os-stat-rdev
	   :os-stat-size
	   :os-stat-uid
	   :os-find
	   :os-cat
	   :os-pwd
	   :os-getcwd
	   :os-mkdir
	   :os-mv
	   :os-cp
	   :os-rm
	   :os-touch
	   :os-ln
	   :os-ls
	   :os-ls-s
	   :os-getenv
	   :os-setenv
	   :os-getenvp
	   :os-architecture
	   :os-realpath
	   :os-env
	   :os-exec
	   :os-kill
           ;;
           :mypid
           :myuid
           :mygid
           :mygroup
           :process-parent-pid
           :convert-signal
           :list-pids
           :parent-pid
           :pid-uid
           :pid-name
           :pid-status
           :pid-env
           :pid-command
           :list-user-pids
           ;; parser
           :parse-args
           :make-argument-parser
           :add-argument
           :get-parsed-argument-value
           ;;
           :flatten
           :tolist
           :tostr
           :tosymbol
           :tokeyword
           ;; concatenation
           :concat-strings
           :concat-symbols
           :concat-keywords
           :concat-symbols-w-delimiter
           :concat-keywords-w-delimiter
           ;; from jabs-external-program
           :start
           :run
           :signal-process
           :process-id
           :process-input-stream
           :process-output-stream
           :process-error-stream
           :process-status
           :process-p
           ;; REST
           :merge-n-directories
           :get-cars
           :scan-all-to-list
           :list-files-recursively
           ;; namespaces
           :namespace-subtree-p
           :add-namespace-subtree
           :replace-namespace-subtree
           ;;
           :bind-jabs-cli-parameter
           :process-jabs-cli-parameter
           :option-set-bool
           :bool-option-p
           ;;
           :find-file-from-list-by-filename
					 ;;
					 :implementation-signature
					 :try
           ;;
           :defun*
           :defgeneric*
           :defmethod*
           ))

(defpackage :jabs
  (:use :cl :tools@jabs :re@jabs)
  (:export :*jabs-buildfile*
           :*jabs-cli-actions*
           :*jabs-currenct-compiler*
           :*jabs-output-log*
           :*jabs-error-log*
           :*fail-on-error*
           :*jabs-source-directory*
           :*jabs-lib-directory*
           :*jabs-share-directory*
           :*jabs-config-directory*
           :*jabs-local-directory*
           :*jabs-local-share-directory*
           :*jabs-local-config-directory*
           :*jabs-local-lib-directory*
           :*jabs-local-src-directory*
	   :+jabs-run-directory+
           ;;
           :tosymbol
           :add-hook
           ;;
           :*post-init-hook*
           :*pre-run-project-hook*
           :*post-run-project-hook*
           :*run-project-hook*
           ;;
           :insert-hit
           :append-hit
           ;;
           :with-project-to-be-run
           :share-plugin
	   ;;
	   :+jabs-version+
	   :*jabs-universal-delimiter*
	   :define-plugin-type
	   :filter-project-plugins-by-type
	   :plugins-api-call-to-true
           ))
