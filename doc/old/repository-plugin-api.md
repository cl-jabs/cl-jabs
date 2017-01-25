method of repository-plugin child class list-reposotiries (plugin) ;; optional
method of repository-plugin child class register-repository (repository plugin) ;; optional
method of repository-plugin child class make-repository (plugin name &key mode uri username password privkey path version) ;; optional
method of repository-plugin child class find-repository (name plugin) ;; optional

method of repository child class list-artifacts (plugin repository) ;; optional
method of repository child class repository-info (plugin repository) ;; required
method of repository child class update-repository (plugin repository) ;; required
method of repository child class find-artifact (artifact-name plugin repository) ;; required
method of repository child class load-artifact (artifact-name plugin repository) ;; required
method of repository child class artifact-info (artifact-name plugin repository) ;; required
method of repository child class artifact-dependencies (artifact-name plugin repository) ;; required
method of repository child class fetch-artifact (artifact-name plugin repository) ;; required
method of repository child class install-artifact (artifact-name plugin repository) ;; required
method of repository child class uninstall-artifact (artifact-name plugin repository) ;; required
method of repository child class upload-artifact (artifact-name plugin repository) ;; required

ex.:

(define-repository-plugin "quicklisp" :uri "https://beta.quicklisp.org/quicklisp.lisp" :path "/tmp" :version "latest"
  ((repo-status :accessor ql-repo-status :iniarg :repo-status))) ;; additional slots for class

(define-repo-api find-artifact quicklisp (plugin repository artifact)
  (ql:system-apropos "^cl-ppcre$")
  
