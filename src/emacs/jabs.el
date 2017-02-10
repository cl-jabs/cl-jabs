;;; jabs.el --- minor mode for editing JABS buildfiles and operate with jabs

;; Copyright (C) 2016 Alexander aka 'CosmonauT' Vynnyk

;; Author: Alexander Vynnyk <cosmonaut.ok@zoho.com>
;; Created: 04 Jul 2016
;; Version: 0.1
;; Keywords: jab jabs

;; This file is NOT part of GNU Emacs.

;;; License:

;; Copyright (c) 2016, Alexander Vynnyk <cosmonaut.ok@zoho.com>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Neither the name of the copyright holder nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL MACIEJ PASTERNACKI BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;; TODO

;;; Code:

;;;###autoload
(define-minor-mode jabs-mode
  "add some bindings to eval code into a running jabs."
  :global nil
  :lighter " JABS"
  ;; :keymap (let ((m (make-sparse-keymap)))
  ;;           (define-key m (kbd "C-M-x") 'stumpwm-eval-defun)
  ;;           (define-key m (kbd "C-x C-e") 'stumpwm-eval-last-sexp)
  ;;           m))
  )


(defcustom jabs-jab-command "jab"
  :group jabs
  "``jab'' command to run")

(defcustom jabs-buildfile "build.jab"
  :group 'jabs)

(defcustom jabs-compiler "sbcl"
  :group 'jabs
  :type 'boolean)

(defcustom jabs-compiler-options ""
  :group 'jabs
  :type 'boolean)

(defcustom jabs-build-recursive-p nil
  :group 'jabs
  :type 'boolean)

(defcustom jabs-log-level nil
  :group 'jabs
  :type 'string)

(defcustom jabs-logfile nil
  :group 'jabs
  :type 'boolean)

(defcustom jabs-error-logfile nil
  :group 'jabs
  :type 'boolean)

(defcustom jabs-excluded-dirs nil
  :group 'jabs
  :type 'boolean)

(defcustom jabs-path-to-compiler nil
  :group 'jabs
  :type 'boolean)

(defcustom jabs-default-bout nil
  :group 'jabs
  :type 'boolean
  )

(defcustom jabs-use-hit-dependencies nil
  :group 'jabs
  :type 'boolean
)

(defcustom jabs-fail-on-error-p nil
  :group 'jabs
  :type 'boolean
  )

(defcustom jabs-fail-on-critical-p t
  :group 'jabs
  :type 'boolean
  )

;; -e, --eval                   Eval expression before of after build
;; --nodeps                     Do not use hit dependencies resolution (working only with --deps-strategy)
;; --no-bouts                   Use bout-based hit running strategy
;; -D<some-parameter>           Give direct parameter to JABS (ex. jab -Dversion returns JABS version, but not JAB)


(defvar jabs-map (make-sparse-keymap)
  "Key map for jabs")

;; (define-key jabs-map (kbd "\C-c \C-k") 'knife)
;; (define-key jabs-map (kbd "\C-c \C-c") 'chef-knife-dwim)

;; (define-minor-mode jabs
;;   "Mode for interacting with Opscode Chef"
;;   nil jabs-map)

;; (defun turn-on-jabs ()
;;   "Enable jabs."
;;   (jabs 1))

;; (define-globalized-minor-mode global-jabs
;;   jabs turn-on-jabs)

;; (defun find-chef-root (&optional path)
;;   (when (null path)
;;     (setq path (or buffer-file-name
;;                    default-directory)))
;;   (cond
;;    ((not (file-directory-p path))
;;     (find-chef-root (concat (file-name-as-directory path) "..")))
;;    ((equal (expand-file-name path) (expand-file-name "~")) nil)
;;    ((equal (expand-file-name path) "/") nil)
;;    ((let ((ff (directory-files path)))
;;       (or (member ".chef" ff)
;;           (and (member "cookbooks" ff)
;;                (member "roles" ff)
;;                (member "config" ff))))
;;     (file-name-as-directory (expand-file-name path)))
;;    (t (find-chef-root (concat (file-name-as-directory path) "..")))))

;; (defun chef/fallback (trigger)
;;   (let* ((jabs nil)
;;          (command (key-binding trigger)))
;;     (when (commandp command)
;;       (call-interactively command))))

;; (defmacro chef/with-root-or-fallback (trigger &rest body)
;;   `(let ((chef-root (find-chef-root)))
;;      (if (null chef-root)
;;          (chef/fallback ,trigger)
;;        ,@body)))

;; (defun chef-run-knife (command &rest args)
;;   (when chef-use-rvm
;;     (rvm-activate-corresponding-ruby))

;;   (let ((knife-buffer (get-buffer-create "*knife*"))
;;         (use-bundler (and chef-use-bundler (file-exists-p "Gemfile"))))
;;     (with-current-buffer knife-buffer
;;       (setq default-directory chef-root)
;;       (setq list-buffers-directory default-directory)
;;       (toggle-read-only 0)
;;       (erase-buffer)
;;       (insert-string (concat "# " default-directory "\n"
;;                              (when use-bundler
;;                                "bundle exec ")
;;                              chef-knife-command " " command " "
;;                              (mapconcat 'identity args " ")
;;                              "\n\n")))
;;     (if use-bundler
;;         (apply 'call-process
;;                "bundle" nil knife-buffer
;;                "bundle" "exec" chef-knife-command (cons command args))
;;       (apply 'call-process
;;              chef-knife-command nil knife-buffer
;;              chef-knife-command (cons command args)))
;;     (with-current-buffer knife-buffer
;;       (toggle-read-only 1))
;;     (switch-to-buffer-other-window knife-buffer t)
;;     (fit-window-to-buffer)))

;; (defun knife (command)
;;   "Run knife"
;;   (interactive "Command: knife ")
;;   (chef/with-root-or-fallback
;;    (kbd "\C-c \C-k")
;;    (apply 'chef-run-knife (split-string-and-unquote command))))

;; (defun chef-knife-dwim ()
;;   "Upload currently edited thing to the Chef server.

;; Guesses whether you have "
;;   (interactive)
;;   (chef/with-root-or-fallback
;;    (kbd "\C-c \C-c")
;;    (let ((b (current-buffer)))
;;      (save-some-buffers nil (lambda ()
;;                               (eq b (current-buffer)))))
;;    (if buffer-file-name
;;        (let ((default-directory chef-root)
;;              (rpath (file-relative-name buffer-file-name chef-root)))
;;          (cond
;;           ((string-match "^\\(?:site-\\)?cookbooks/\\([^/]+\\)/" rpath)
;;            (print (match-string 1 rpath))
;;            (chef-run-knife "cookbook" "upload" (match-string 1 rpath)))
;;           ((string-match "^\\(role\\|node\\|environment\\)s/\\(.*\\)" rpath)
;;            (chef-run-knife (match-string 1 rpath) "from" "file" (match-string 2 rpath)))
;;           ((string-match "^data.bags/\\([^/]+\\)/\\(.*\\.yaml\\)" rpath)
;;            (chef-run-knife "data" "bag" "from" "yaml" (match-string 1 rpath) (match-string 2 rpath)))
;;           ((string-match "^data.bags/\\([^/]+\\)/\\(.*\\)" rpath)
;;            (chef-run-knife "data" "bag" "from" "file" (match-string 1 rpath) (match-string 2 rpath)))
;;           (t (chef/fallback (kbd "\C-c \C-c")))))
;;      (chef/fallback (kbd "\C-c \C-c")))))



;; (defun chef-resource-lookup ()
;;   "Open the documentation in a browser for the chef resource at point"
;;   (interactive)
;;   (let* ((base "http://wiki.opscode.com/display/chef/Resources")
;;         (anchor "#Resources-")
;;         (tbl '(
;;                ("cookbook_file" . "CookbookFile")
;;                ("cron" . "Cron")
;;                ("deploy" . "Deploy")
;;                ("directory" . "Directory")
;;                ("env" . "Env")
;;                ("erl_call" . "ErlangCall")
;;                ("execute" . "Execute")
;;                ("file" . "File")
;;                ("git" . "Git")
;;                ("group" . "Group")
;;                ("http_request" . "HTTPRequest")
;;                ("ifconfig" . "Ifconfig")
;;                ("link" . "Link")
;;                ("log" . "Log")
;;                ("mdadm" . "Mdadm")
;;                ("mount" . "Mount")
;;                ("ohai" . "Ohai")
;;                ("package" . "Package")
;;                ("apt_package" . "Package")
;;                ("dpkg_package" . "Package")
;;                ("easy_install_package" . "Package")
;;                ("freebsd_package" . "Package")
;;                ("macports_package" . "Package")
;;                ("portage_package" . "Package")
;;                ("rpm_package" . "Package")
;;                ("gem_package" . "Package")
;;                ("yum_package" . "Package")
;;                ("zypper_package" . "Package")
;;                ("powershell" . "PowerShellScript")
;;                ("remote_directory" . "RemoteDirectory")
;;                ("remote_file" . "RemoteFile")
;;                ("route" . "Route")
;;                ("ruby_block" . "RubyBlock")
;;                ("scm" . "SCM")
;;                ("script" . "Script")
;;                ("bash" . "Script")
;;                ("csh" . "Script")
;;                ("perl" . "Script")
;;                ("python" . "Script")
;;                ("ruby" . "Script")
;;                ("service" . "Service")
;;                ("subversion" . "Subversion")
;;                ("template" . "Template")
;;                ("user" . "User")
;;                ))
;;         (target (assoc-string (symbol-at-point) tbl)))

;;   (if target
;;       (browse-url (concat base anchor (cdr target)))
;;     (browse-url base))))

(defun jabs-generate-buildfile ()
  (interactive)
  (call-process (concat jabs-jab-command " -b " jabs-buildfile)))

(provide 'jabs)
;;; jabs.el ends here
