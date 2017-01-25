# Service functions and variables

**also, see [OS layer functions](os-layer-functions.md)**

## Package: jabs-core

* `*jabs-default-skelethon*` - set default skelethon, which used
* `+jabs-version+` - show JABS version
* `*jabs-output-log*` - output logfile (string or pathname)
* `*jabs-error-log*` - error logfile (string or pathname)

### System variables

You can use this variables, when you write your own hits. They will be set by JABS separately for each project
* `*jabs-current-project*` - service variable. Can be used in hit definitions to use as current project instance
* `*jabs-current-skelethon*` - service variable. Can be used in hit definitions to use as current skelethon instance
* `*jabs-current-pathname*` - service variable. Can be used in hit definitions to use as current project path
* `*jabs-current-bout*` - service variable. Can be used in hit definitions to use as current bout instance
* `*current-project*` - service variable. Can be used in hit definitions to use as current project instance
* `*current-skelethon*` - service variable. Can be used in hit definitions to use as current skelethon instance
* `*current-bout*` - service variable. Can be used in hit definitions to use as current bout instance
* `*current-sources*` - service variable. Can be used in hit definitions to use as current source pathnames list
* `*current-repositories*`  - service variable. Can be used in hit definitions to use as current project repositories list (v.0.2+) 
* `*current-hits*` - service variable. Can be used in hit definitions to use as current hits list 

* `*jabs-quiet*`, `*jabs-verbose*`, `*jabs-debug*` - set verbosity/debug level

* `*jabs-buildfile*` - set name of buildfile
* `+jabs-configfile+` - jabs system configfile

### Hooks
* `add-hook`
* `run-hook`
* `*register-source-hook*`

### Service functions

* `concatenate-to-string-with-delimiter` - make string, like `(concatenate-to-string-with-delimiter "-" "foo" "bar" "baz") => "foo-bar-baz"
* `make-jabs-symbol` - make symbol from strings
* `concatenate-symbol` - concatenate strings to symbol
* `find-compiler` - find compiler definition object
* `msg` - universal message macro. `(msg :err|:warn|:msg|:info|:dbg "format macro syntax message")`.
* `with-msg` - macro, which used to exec some code with log message before
* `with-dbg` - macro, which used to exec some code with log debug message before 
* `with-info` - macro, which used to exec some code with log info message before 
* `with-warn` - macro, which used to exec some code with log warning before
* `with-err` - macro, which used to exec some code with log error before

* `terminate` - terminates JABS with status Ex.: `(terminate 1)`

* `file-exists-p` - checks if file exists
* `pathname-as-directory` - makes smth like "/home/test" to "/home/test/"
* `maphash-keys` - like `maphash`, but only with keys
* `list-directory` - lists files in some directory
* `directory-pathname-p` - checks if pathname is directory
* `pathname-as-file` - - makes smth like "/home/test/" to "/home/test"

* `merge-n-directories`
* `*jabs-bout-registry*`
* `*jabs-bouts-directory*`
* `*jabs-bout-template-type*`
* `*jabs-default-bout*`
* `*jabs-hit-registry*`
* `*jabs-hits-directory*`
* `*jabs-hit-template-type*`
* `*jabs-skelethon-subdir*`
* `*fail-on-error*` - Drop to debug loop, when error messaged (see `msg`)

## Package: jabs

* `register-plugin` - v.0.2+
* `register-repository` - v.0.2+
* `make-repository-plugin-name` - v.0.2+
* `find-repository-plugin-package` - v.0.2+
* `repository-plugin-registered-p` - v.0.2+
* `call-api-from-repository-plugin-package` - v.0.2+
* `find-repository` - v.0.2+
* `get-repository-type` - v.0.2+
* `get-repository-names-list` - v.0.2+
* `make-repository-plugin-name` - v.0.2+
* `load-plugin` - v.0.2+
* `repository` - v.0.2+
* `defrepo` - v.0.2+

* `defskelethon` - define skelethon
* `defhit` - define hit
* `defproject` - define project
* `defbout` - define bout

### Project directories getter

* `project-src-dir`
* `project-bin-dir`
* `project-lib-dir`
* `project-doc-dir`
* `project-share-dir`
* `project-test-dir`
* `project-contrib-dir`
* `project-conf-dir`
* `project-public-dir`
* `project-cache-dir`
* `project-script-dir`
* `project-log-dir`
* `project-template-dir`
* `project-opt-dir`
* `project-tmp-dir`

## Package:  jabs-repository-api
* `uninstall-package` - v.0.2+
* `update-package` - v.0.2+
* `package-status` - v.0.2+
* `package-version` - v.0.2+
* `packages-list` - v.0.2+
* `refresh-repository` - v.0.2+
* `initialize-repository` - v.0.2+
* `package-list-dependencies` - v.0.2+
