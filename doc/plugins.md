# Plugins

JABS can be extended with external plugins. It's plugins for different tasks, like download dependencies from external repositories, perform testing and test reports, integrate JABS with other systems, and more and more. There is a set of plugins, which available out-of-the-box. But, of course, you can write your own plugins.

## Plugin types

Every plugin must be one, of available types. Plugin type means, that plugin must pass some requirements, predefined for this plugin type. It could be requirement to realize some API, has some functions of variables in plugin body etc. This requirements checks right after plugin loaded. There are several builtin plugin types, but, of course, user can define his/her own plugin types. Custom plugin types can be defined in other plugins, or directly in [build.jab](project.md#build.jab).

#### Custom plugin type definition

Custom plugin type can be defined with `define-plugin-type` macro. Syntax:

```lisp
(define-plugin-type :custom-plugin variable-for-plugin-object
  ;; define name of plugin
  (let ((plugin-package-name
	 (eval (append '(concatenate-symbol *jabs-universal-delimiter*)
		       (list (get-plugin-name plugin))
		       (list (get-plugin-type plugin))
		       +jabs-plugin-namespace+))))
    ;; check, if package with plugin name exists
    (find-package plugin-package-name)))
```

Real example:

```lisp
(define-plugin-type :repository plugin
  (let ((checker t)
	(plugin-package-name
	 (eval (append '(concatenate-symbol *jabs-universal-delimiter*)
		       (list (get-plugin-name plugin))
		       (list (get-plugin-type plugin))
		       +jabs-plugin-namespace+))))
    ;; TODO: check for functions format (args and their types)
    (dolist (v '(:initialize-repository :find-repository-project
					:load-repository-project :remove-repository-project
					:repository-project-version :update-project-repository-projects
					:repository-project-dependencies))
      (when (not (try (symbol-function (tosymbol v plugin-package-name))))
	(setf checker nil)))
    checker))
```

## Predefined plugin types



| Type        | Description |
|:------------|:------------|
| core        | Service type for core plugins |
| generic     | Type for genecit usage. No special requirements |
| repository  | Type for dependencies repositories management |
| test        | Type for plugins to perform testing with dieerent test frameworks/engines |
| test-report | Type to generate test reports |
| package     | Type to perform project packaging to prepare to it's deployment to local or remote repository|
| deployment  | Type to perform projects deployment to some type of binary packages repository |

## Plugin names and marking

Plugin names are hierarchical and contains plugin name, type and plugin namespace, delimited by [JABS Universal Delimiter](), or just `at` symbol: `@`. So, typical name is:
```lisp
my-plugin@my-plugin-type@plugin@jabs
```

## Define plugin

You must input three required arguments after `defplugin` calling: 1st - name, 2nd - type, 3rd - version. All other arguments are optional

```lisp
(defplugin :my-plugin :my-type "0.1"
  :author "John Doe <email@null>"
  :maintainer ""
  :license "Public Domain"
  :description "Here is my project"
  ;; :serial t
  :skelethon (:flat)
;;; Dependencies
  :depends-on (:alexandria :cl-ppcre)
  :pathname "the/relative/path/name/to/your/project/root/directory"
;;; Add files, mobules etc
  :components ((:file "my-file")))
```

Fill free to use JABS project syntax (except bouts - it's not working for plugins) as well as ASDF syntax.

### Files and directories

Plugin organized, like any other JABS [project](project.md) and defines in `build.jab` file, as any other JABS project. Also, you can use [skelethons](plugins/core/jabs-skelethon.md), `dependencies`, `components` etc.

## Generate plugins automatically

JABS has builtin plugin to generate projects and plugins from pre-defined templates: [tmpl@generic](plugins/generic/tmpl.md). So, you can use it:

```bash
cd to/your/plugin/directory
jab tmpl-mkplugin -Dplugins=tmpl@generic -Dtmpl-skelethon=default
```

## Also

You can find working plugin example [here](examples/plugin/)
