# make-binary@generic

Makes binary executable from current lisp program in skeleton binary directory

## Usage

* Add plugin dependency to your project

```lisp
(defproject :foo
  ...
  (make-binary (:name "some_name" :toplevel-function "package:function")
  ...
```

* Run JABS with `make-binary` bout

```lisp
jab @make-binary
```