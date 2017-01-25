# TODO

## Main:
* insert/append-hit/round - FIX
* Fix logging [DONE]
* препроцесор
* jab
  * компілятори для jab
* портування на ccl
* портування на clisp
* фікс quicklisp-а: перемістити бібліотеки у tmp [DONE] (new skelethon dir: cache)
* заюзати *jabs-source-registry* (?)
* зробити можливість логування у файл (?)
* виставити нормально рівні логування (0-5)
* повний рефакторинг коду [DONE]
* make tests for JABS
* enable ASDF integration **[DONE]**
* clean

## Plugins:
* Core:
  * dependency-manager - core [DONE]
  * test-launcher - core 
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
  * quicklisp repository **[DONE]**

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

## Longterm:
* port to clisp/cmucl/ccl
* artifacts repository for (like nexus)

## Issues
BUG:
CURRENT: loading plugin dependencies from 'root' directory of project. Not from plugin root directory' (or using main project's skelethon).
REQUIRED: load plugin dependencies, using current project's skelethon

2017-01-23 19:58:56 [DEBUG]: Registering plugin ``FIVEAM@TEST@PLUGIN@JABS''
2017-01-23 19:58:56 [INFO]: Plugin ``FIVEAM@TEST@PLUGIN@JABS'', type ``TEST'' registered
2017-01-23 19:58:56 [DEBUG]: Creating project ``FIVEAM@TEST@PLUGIN@JABS''
2017-01-23 19:58:56 [DEBUG]: Registering project ``FIVEAM@TEST@PLUGIN@JABS''
2017-01-23 19:58:56 [DEBUG]: Processing skelethon file ``/home/cosmonaut/dev/cl-jabs/share/skelethons/flat.skl''
2017-01-23 19:58:56 [DEBUG]: Registering skelethon ``FLAT''
2017-01-23 19:58:56 [INFO]: Skelethon ``FLAT'' registered
2017-01-23 19:58:56 [DEBUG]: Registering ASDF system ``FIVEAM@TEST@PLUGIN@JABS''
2017-01-23 19:58:56 [INFO]: Project ``FIVEAM@TEST@PLUGIN@JABS'' registered
2017-01-23 19:58:56 [DEBUG]: Running ``*PRE-LOAD-PLUGIN-HOOK*'' hook
2017-01-23 19:58:56 [DEBUG]: Loading plugin ``FIVEAM@TEST'' dependencies
Unhandled SB-INT:SIMPLE-READER-PACKAGE-ERROR in thread #<SB-THREAD:THREAD
                                                         "main thread" RUNNING
                                                          {10039CEC63}>:
  Package QL-INFO does not exist.

    Line: 6, Column: 25, File-Position: 92

    Stream: #<SB-SYS:FD-STREAM
              for "file /home/cosmonaut/dev/cl-phinan/cache/quicklisp/cl-phinan/quicklisp/quicklisp.asd"
              {100706B6D3}>


FEATURE:
REQUIRED: make plugin dependencies (like: :depends-on `(alexandria fiveam@test@plugin@jabs)` or smth).

====================================================================================================
  
NOTES:
http://lisper.ru/wiki/libraries:quicklisp - about quicklisp
https://www.darkchestnut.com/2016/dont-put-quicklisp-in-your-binary-just-because-a-library-demands-it/

====================================================================================================

