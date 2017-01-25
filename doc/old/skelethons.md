# Skelethons

## Definition

You can define skelethons in 4 places:
* directly in `defproject` project definition in buildfile (`build.jab` by default.) [see the project documentation](doc/projects.md)
* directly in buildfile as separate definition `defskelethon`
* in local skelethons direcotory: `<project_path>/.jabs/share/skelethons/<hit_name>.hit`
* in system skelethons directory: `<jabs_share_dir>/skelethons/<hit_name>.hit`

## Syntax

Skelethons has 2 types of directories and files: required and optional. So, if you initializing skelethon, you can initialize just only required direcotories or initialize all directories forced

* inline in `defproject`:
```
(defproject :test-project
...
:skelethon (:foo-skelethon :src "src" :etc "etc")
...
)
```

* as `defskelethon`
```
(defskelethon :baz-skelethon
  :src ("src" :required) ;; TODO: add working mode definition
  :lib "lib"
  :doc ("doc" :required)
  :conf "conf"
  :test ("src/tests" :required)
  :share "share"
  :contrib "contrib"
  :script "script"
  :readme-file "README"
  :license-file "LICENSE"
  :install-file "INSTALL")

```
* in separate file (in local or system skelethons directory)

```
(qux-skelethon
  :src ("src" :required) ;; TODO: add working mode definition
  :lib "lib"
  :doc ("doc" :required)
  :conf "conf"
  :test ("src/tests" :required)
  :share "share"
  :contrib "contrib"
  :script "script"
  :readme-file "README"
  :license-file "LICENSE"
  :install-file "INSTALL")
```
