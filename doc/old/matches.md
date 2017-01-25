# Matches

## Definition

You can define bouts in 4 places:
* directly in `defproject` project definition in buildfile (`build.jab` by default.) [see the project documentation](doc/project.md)
* directly in buildfile as separate definition `defbout`
* in local bouts direcotory: `<project_path>/.jabs/share/bouts/<bout_name>.mch`
* in system bouts directory: `<jabs_share_dir>/bouts/<bout_name>.mch`

## Syntax

You can define bout globally, of just for one project, and for all projects

* inline in `defproject`:
```
(defproject :test-project
...
:bout (:verify :compile :clean)
...
)
```

* as `defbout`
```
(defbout :baz-bout ;; leave second argument empty for global bouts
  (:phases :verify :compile :clean))
```
* in separate file (in local or system bouts directory)

```
(:qux-bout
  :validate
  :compile
  :clean)
```
