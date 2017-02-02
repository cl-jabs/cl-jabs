# TODO

## Main:
* preprocessor (?)
* jab **[PARTLY]**
  * compilers for `jab`
* зробити можливість логування у файл (?)
* виставити нормально рівні логування (0-5)
* make tests for JABS
* clean
* add profiling
* launch single round or hit
* realise round list view for projects
* remove *jabs-verbose* variable as deprecated
* remove *jabs-quiet* variable as deprecated
* remove *jabs-debug* variable as deprecated
* handle exception, when round does not exists in bout
* make automatic detection, which project should be launched
* check for multiple directories to load libs/plugins/skelethons/bouts/rounds/hits etc
* make possibility to add absolute pathnames to skelethon definition
* make skelethon definition in project regular (now: `:skelethon (:default)` )

## Plugins:
* Core:
  * test-launcher - core **[PARTLY]**
  * deployment-manager - core
  * documentation(reporting?) - core

* Generic:
  * swank/slime - generic
  * scm - generic
  * patch(?) - generic
  * binary - generic
  * simple http client
  * external program (core?)

* Repository:
  * github - repository
  * git repository
  * bitbucket repository

* Report:
  * Style report plugin
  * changelog
  * doc
  * autodoc - generate document with all exported functions and documentation to them
  * project-info-reports - generate standard project reports
  * test-report - generate reports about result of running tests

* Packaging:
  * zip - packaging
  * tar.gz - packaging
  * deb - packaging
  * rpm - packaging

## Wrappers:
* jab
* jabslime

## Longterm:
* port to clisp/cmucl/ccl
* artifacts repository for (like nexus)

====================================================================================================
  
NOTES:
http://lisper.ru/wiki/libraries:quicklisp - about quicklisp
https://www.darkchestnut.com/2016/dont-put-quicklisp-in-your-binary-just-because-a-library-demands-it/
