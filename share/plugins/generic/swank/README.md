# SWANK plugin

```lisp
(defproject :foo
...
:plugins (... :swank@generic ...)
...
:swank (:port 4010 :host "0.0.0.0")
```

CLI parameters: swank-host, swank-port

## Bouts
* swank - loads project, than launches SWANK
* swank-dev - loads project loads and runs tests, than launches SWANK

## Run
```bash
jab @swank -Dswank-port=2222 -Dswank-host=0.0.0.0
```

```bash
jab @swank-dev -Dswank-port=2222
```

