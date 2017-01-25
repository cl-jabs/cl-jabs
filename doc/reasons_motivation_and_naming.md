# Reasons and motivation

## Questions

Q: Why not ASDF?
A: ASDF is a tool, which have restricted possibilities to perform phases or modern software lifecycle. It can't be simply extended and integrated with external tools. There is no builtin defsystem mechanism to execute tests (you must create another system). There is no mechanism to execute just selected tests. There is no simple mechanism to recompose your build flow or create custom and so on and so on. So, JABS designing and developing to solve most of asdf problems.

Q: Why not quicklisp?
A: It's very special tool, which can just get and load packages. It's very good idea to integrate it to build system. So, you should define smth. like
```lisp
(defsystem
  ...
  (depends-on :alexandria)
  ...
  )
```
...and voila! Package will be get from some external repository and loaded to runtime.

Q: Why not ADSF+quicklisp?
A: Every time, when I use ASDF+quicklisp, I write a big piece of code to integrate them to each other. It's really painful to have own "libscotch" to combine asdf and quicklisp and attach it to your every project :) Also, this combination can not run tests, as it does modern build systems (ex. per-file, per-suite, single test or several different tests etc), prepare code to build/deployment (ex. copy files, create directories, create files from templates etc). Of course, you can do it with autotools/Makefile. But, what if we have pure common lisp tool, which can do it all?

Q: Why `project`, but not traditional `system`?
A: Because `project` is not a `system` ;) Project is back compatible with asdf system, but it's quite different things. Ex. project has no stable grammar. It could be extended at your wish. In project you can describe, how to run it etc.

Q: Can I still use my ASDF systems wich JABS?
A: Yes. JABS is integrated with ASDF, so you `defsystem` in JABS is just a macro, which creates JABS project and ASDF system. So, ASDF `defsystem` becomes a `defproject` with `flat` skelethon (for back compatability). And, yes, you can load `*.asd` files with JABS as you did it with ASDF before.

Q: Can I still run my tests, defined for ASDF as part as new flow?
A: Yes, but you should define `asdf@test` in your `plugins` section of your project

## Conventions
