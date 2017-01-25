JABS is project of modern build system for common lisp, inspired by ASDF, autotools and maven build systems. It extends possibilities
of de-facto CL build system ``asdf`` with many things, like build bout, additional inline source paths definition, automatic
dependencies loading, external plugins etc. JABS has plugin interface, which allows to use additional plugins. Also, JABS gives
repository, deployment and packaing API, which allows to download dependencies automatically, package it and deploy to remote hosts,
using external plugins and just including it to your project. Also, JABS has handy CLI with the ``jab`` util, which performs basic
operations with JABS.

Also, JABS has back compatability with asdf`s ``defsystem``, but, of course, it will process classical system without most of new features

What IS JABS?

- JABS is an abbreviation for for Just Another Build System

- JABS uses MAVEN-like terms and definitions: it has ``hits`` (maven goals), ``projects`` (maven project), ``bouts`` (maven
  bout), ``skelethons`` (like maven directory layer, but more flexable).









ASDF will not download missing software components for you. For that, you want Quicklisp, that builds upon ASDF, and is great for pulling and installing tarballs of packages you may depend upon; we also recommend clbuild, that now builds upon Quicklisp, as a great tool for pulling from version control packages you need to modify or want to contribute to. We recommend you should not use asdf-install anymore, as it is an older similar piece of software that is both unmaintained and obsolete.

ASDF is also not a tool to build or run Common Lisp software from the Unix command-line. For that, you want cl-launch, or perhaps buildapp.

If you're unsatisfied with ASDF, beside helping with our TODO list, you might be interested in other build systems for Common-Lisp:


What IS NOT JABS?

- JABS is not searching for project files recursively. This performed by ``jab`` util

- JABS has no ``archetypes`` or ``skaffolding``



* Support:
abcl no
ccl no
clasp no
clisp no
cmucl no
ecl no
mkcl no
sbcl yes
gcl no
mcl no
xcl no

allegro
lispworks no
cormanlisp no
genera no
mocl no
scl no



* Getting it


* Extensions

* Contributing

Join our mailing list, check the code out from git, send questions, ideas and patches!

* Reporting Bugs

To report bugs, you can use our launchpad project. If you're unsure about the bug or want to discuss how to fix it, you can send email to the project mailing-list below.
Mailing Lists

    asdf-devel A list for questions, suggestions, bug reports, patches, and so on. It's for everyone and everything. Please join the conversation! asdf-devel mailman site to subscribe
    asdf-announce A low-volume mailing-list for announcements only, mostly regarding new releases. Posting is restricted to project administrators and to important notices. Please subscribe to it if you're a Lisp implementation or distribution vendor, who needs to know when to upgrade the ASDF you distribute, but are otherwise not interested in day to day design and development. asdf-announce mailman site to subscribe 

* Contributing

Join our mailing list, check the code out from git, send questions, ideas and patches!

* What is happening

