# TODO

## Main:
* preprocessor (?)
* jab **[PARTLY]**
  * compilers for `jab`
* make feature logging to file (?)
* fix loglevels (0-5)
* make tests for JABS
* clean
* add profiling
* launch single round or hit
* realise round list view for projects
* remove `*jabs-verbose*`, `*jabs-quiet*` and `*jabs-debug*` variables as deprecated
* make automatic detection, which project should be launched
* make possibility to add absolute pathnames to skeleton definition
* add CLI option additional-plugins

## Plugins:
* Core:
  * test-launcher - core **[PARTLY]**
  * deployment-manager - core
  * documentation(reporting?) - core

* Generic:
  * swank/slime
  * scm
  * patch(?)
  * simple http client
  * external program (core?, tools?) - run external program

* Repository:
  * github
  * git
  * bitbucket

* Report:
  * Style report plugin
  * changelog
  * doc
  * autodoc - generate document with all exported functions and documentation to them
  * project-info-reports - generate standard project reports
  * test-report - generate reports about result of running tests

* Packaging:
  * zip
  * tar.gz
  * quicklisp
  * deb
  * rpm

## Wrappers:
* jab
* jabslime

## Longterm:
* port to clisp/cmucl/ccl
* artifacts repository (like nexus. Integrate with nexus?)
