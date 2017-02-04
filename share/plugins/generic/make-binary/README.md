# make-binary@generic

Makes binary executable from current lisp program in skelethon binary directory

## Usage

```lisp
(defproject :foo
  ...
  (make-binary (:name "some_name" :toplevel-function "package:function")
  ...
```
