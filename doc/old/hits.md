# Targets

## Definition

You can define hits in 2 places:
* directly in `defproject` project definition in buildfile (`build.jab` by default.) [see the project documentation](doc/project.md)
* directly in buildfile as separate definition `defhit`
* in local hits direcotory: `<project_path>/.jabs/share/hits/<hit_name>.hit`
* in system hits directory: `<jabs_share_dir>/hits/<hit_name>.hit`

## Syntax

You can define hit globally, of just for one project, and for all projects

* inline in `defproject`:
```
(defproject :test-project
...
:hits ((:foo-hit :depends-on (:bar-hit) (here (will (be (some code))))))
...
)
```

* as `defhit`
```
(defhit :baz-hit (:test-project) ;; leave second argument empty for global hits
  :depends-on (:foo-dep-hit :bar-dep-hit)
  (here (will (be (the code)))))
```
* in separate file (in local or system hits directory)

```
(qux-hit ()
  :depends-on (:qux-dep-hit)
  (here (will (be (the code)))))
```
