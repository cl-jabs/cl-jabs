# Project


## Syntax

### name
### author
### maintainer
### license
### skelethon
### bout
### hits
### pathname
### components

## Example
```
(defproject :foo-project
  :name "FOO-PROJECT"
  :author "Foo Bar <foo.bark@dev.null>"
  :version "1.0"
  :maintainer "Foo Bar <foo.bark@dev.null>"
  :license "GNU General Public License version 3"
  :description "This is just sample project"
  :serial t
  :skelethon (:default :force)
  :bout :default
  :depends-on (:cl-actors)
  :pathname "the/pathname"
  :hits ((:compile :depends-on (:verify) (format t "compilation"))
      (:verify (format t "verification")))
  :components ((:file "package")
               (:file "foo")))
```
