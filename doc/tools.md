# JABS Tools
* `var *jabs-namespace-tree*`

* `var *stdin* *standard-input*`

* `macro try (&body body)`

* ``macro ,def* (name formals &rest rest)`

* `fun setup-stdin ()`

* `var *stdout* *standard-output*`

* `fun setup-stdout ()`

* `var *stderr* *error-output*`

* `fun setup-stderr ()`

* `fun flatten (list)`

* `fun terminate (&optional (status 0))`

* `fun tolist (stuff)`

* `fun tostr (stuff &optional downcase)`

* `fun tosymbol (symbol &optional (package :keyword))`

* `fun tokeyword (symbol)`

* `macro concat-strings (&body strings)`

* `fun concat-symbols (package &rest symbols)`

* `macro concat-keywords (&body symbols)`

* `fun concat-symbols-w-delimiter (delimiter package &rest symbols)`

* `macro concat-keywords-w-delimiter (delimiter &body symbols)`

* `fun argv (&optional argnumber)`

* `fun maphash-keys (function table)`

* `fun maphash-values (function table)`

* `macro if-let (bindings &body (then-form &optional else-form)) ;; from alexandria`

* `fun copy-hash-table (table &key key test size`

* `fun component-present-p (value)`

* `fun directory-pathname-p (pathspec)`

* `fun pathname-as-directory (pathspec)`

* `fun pathname-as-file (pathspec)`

* `fun directory-wildcard (dirname)`

* `fun clisp-subdirectories-wildcard (wildcard)`

* `fun list-directory (dirname &key (follow-symlinks t))`

* `fun file-exists-p (pathspec)`

* `fun directory-exists-p (pathspec)`

* `var *stream-buffer-size* 8192)`

* `fun copy-stream (from to &optional (checkp t))`

* `fun copy-file (from to &key overwrite)`

* `fun delete-directory-and-files (dirname &key (if-does-not-exist :error))`

* `fun get-cars (list)`

* `fun emptyp (x)`

* `fun find-package* (package-designator &optional (error t))`

* `fun find-symbol* (name package-designator &optional (error t))`

* `fun symbol-call (package name &rest args)`

* `fun finish-outputs (&rest streams)`

* `fun featurep (x &optional (*features* *features*))`

* `fun first-feature (feature-sets)`

* `fun ccl-fasl-version ()`

* `fun lisp-version-string ()`

* `fun os-hostname ()`

* `fun parse-unix-namestring* (unix-namestring)`

* `fun os-cd (x)`

* `macro os-chdir (x)`

* `fun os-dirname (pathname)`

* `fun list-files-recursively (dir &optional files)`

* `fun find-by-name (name list)`

* `fun find-by-type (type list)`

* `fun find-directories (list)`

* `fun merge-n-directories (&rest dirs)`

* `fun os-find (path &key name extension (type t))`

* `fun cat-to-string (file)`

* `fun cat-to-list (file &aux result list (rt (copy-readtable)))`

* `fun os-cat (file &optional (output-type :string)) ; types: :string :list`

* `fun os-pwd ()`

* `macro os-getcwd ()`

* `fun os-mkdir (dirname)`

* `macro os-mv (from to)`

* `fun os-cp (from to &key recursive force)`

* `fun os-rm (pathname &key recursive)`

* `fun os-touch (pathname)`

* `fun os-getuid ()`

* `fun os-getenv (x)`

* `fun os-getenvp (x)`

* `fun implementation-type ()`

* `var *implementation-type* (implementation-type)`

* `fun os-architecture ()`

* `fun operating-system ()`

* `fun implementation-identifier ()`

* `fun scan-all-to-list (startsymbol endsymbol target-string &optional collector)`

* `fun subtree (name list)`

* `fun replace-subtree (name list sublist)`

* `fun add-subtree (name list sublist)`

* `macro namespace-subtree-p (&rest name)`

* `macro add-namespace-subtree (subtree &rest name)`

* `macro replace-namespace-subtree (subtree &rest name)`

* `fun find-file-from-list-by-filename (name files)`

* `fun pathname-root (pathname)`

* `fun absolute-pathname-p (pathspec)`

* `fun truenamize (pathname &optional (defaults *default-pathname-defaults*))`

* `fun os-realpath (path)`

* `fun implementation-signature ()`

