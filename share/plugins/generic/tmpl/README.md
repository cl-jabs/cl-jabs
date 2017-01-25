<!-- make-project@templating@generic name=xxx -->
<!-- make-asdf-system@templating@generic -->
<!-- make-plugin@templating@generic -->
<!-- make-bout@templating@generic rounds=a,b,c -->
<!-- make-round@templating@generic hits=d,e,f -->
<!-- make-hit@templating@generic -->

<!-- ==================================================================================================== -->
<!-- Quickproject - create a Common Lisp project skeleton -->

<!-- Quickproject is a library for creating a Common Lisp project skeleton. It is available under a BSD-style license; see LICENSE.txt for details. The latest version is 1.2.2, released on October 1st, 2014. -->

<!-- Download shortcut: http://www.xach.com/lisp/quickproject.tgz -->
<!-- Contents -->

<!--     Overview -->
<!--     Examples -->
<!--     Dictionary -->
<!--         make-project -->
<!--         *author* -->
<!--         *include-copyright* -->
<!--         *license* -->
<!--         *template-directory* -->
<!--         default-template-parameters -->
<!--         *template-parameter-functions* -->
<!--         *after-make-project-hooks* -->
<!--     Feedback -->

<!-- Overview -->

<!-- Quickproject provides a quick way to make a Common Lisp project. After creating a project, it extends the ASDF registry so the project may be immediately loaded. -->
<!-- Examples -->

<!-- * (quickproject:make-project #p"~/src/myproject/" :depends-on '(drakma cxml)) -->
<!-- "myproject" -->

<!-- * (asdf:load-system "myproject") -->
<!-- load output -->

<!-- * (quickproject:make-project #p"~/src/websnarf/" :name "cl-websnarf") -->
<!-- "cl-websnarf" -->

<!-- * (directory #p"~/src/websnarf/*.*") -->
<!-- (#p"~/src/websnarf/README.txt" -->
<!--  #p"~/src/websnarf/package.lisp" -->
<!--  #p"~/src/websnarf/cl-websnarf.asd" -->
<!--  #p"~/src/websnarf/cl-websnarf.lisp") -->

<!-- Dictionary -->

<!-- The following symbols are exported from the quickproject package. -->
<!-- [Function] -->
<!-- make-project pathname &key depends-on author include-copyright license name template-directory template-parameters => project-name -->

<!--     Create the skeleton of a Common Lisp project in directory. If given, name is used as the name of the project. Otherwise, the name is taken from the last component in the pathname-directory of the pathname. For example, the last directory component of #p"src/lisp/myproject/" is "myproject". -->

<!--     The project skeleton consists of the following files: -->

<!--         README.txt -->
<!--         package.lisp — defines a package named after the project -->
<!--         name.asd — defines an ASDF system named after the project, with a :depends-on list as given in the function call -->
<!--         name.lisp -->

<!--     If provided, author and license are used to initialize certain parts of the default files with extra information. The default values are taken from *AUTHOR* and *LICENSE*, respectively. -->

<!--     If provided, the boolean argument to include-copyright will determine whether copyright notices will be printed in the header of each file. -->

<!--     If provided, each file in template-directory is rewritten with HTML-TEMPLATE into the new directory. The options are as follows: -->

<!--         The template markers are (#| and |#) -->
<!--         No escaping is done in template values -->
<!--         Template parameters are created by appending template-parameters with the lists returned by calling each entry in *TEMPLATE-PARAMETER-FUNCTIONS* -->

<!--     After rewriting templates, each element in *AFTER-MAKE-PROJECT-HOOKS* is called. -->

<!--     After the project has been created, its pathname is added to ASDF:*CENTRAL-REGISTRY*, so the project is immediately loadable via ASDF:LOAD-SYSTEM. -->

<!-- [Special variable] -->
<!-- *author* -->

<!--     This string is used to initialize the :author argument in the project system definition. The default initial value is "Your Name <your.name@example.com>". -->

<!-- [Special variable] -->
<!-- *include-copyright* -->

<!--     This variable is used to control whether a copyright notice (with the author's name and the current year) should appear in the header of each file.. -->

<!-- [Special variable] -->
<!-- *license* -->

<!--     This string is used to initialize the :description argument in the project system definition. The default initial value is "Specify license here". -->

<!-- [Special variable] -->
<!-- *template-directory* -->

<!--     If non-NIL, this variable should be bound to a pathname used as the default value of template-directory in MAKE-PROJECT. -->

<!-- [Function] -->
<!-- default-template-parameters => parameters -->

<!--     Return a plist with values for :name, :license, and :author for the current project being created via MAKE-PROJECT. This function is in the default value of *TEMPLATE-PARAMETER-FUNCTIONS*. -->

<!-- [Special variable] -->
<!-- *template-parameter-functions* -->

<!--     A list of functions that are called to produce template parameters when rewriting templates in MAKE-PROJECT. Each function is called with no arguments and should produce a list of keyword/value pairs. The resulting lists are appended together for use as template parameters in HTML-TEMPLATE:FILL-AND-PRINT-TEMPLATE. -->

<!--     The default value is (default-template-parameters). -->

<!-- [Special variable] -->
<!-- *after-make-project-hooks* -->

<!--     A list of designators for functions to be called after a project has been created. Each function should accept one required argument, the pathname given to MAKE-PROJECT, and two keyword arguments, :name and :depends-on, which correspond to the name of the project (whether explicitly supplied to MAKE-PROJECT or derived from the pathname) and the :depends-on argument, respectively. *default-pathname-defaults* is bound to the newly created project pathname when hooks are called. -->

<!-- Feedback -->

<!-- For questions or comments about Quickproject, please email me, Zach Beane <xach@xach.com>. -->

# Launch templating generation
Run project template generation with
```
run.sh -Dplugins=tmpl@generic -Dbouts=tmpl-mkproject
```
