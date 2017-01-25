# Files and directories

## Directories

### System directories

Located at system path (depends on installation)

```
doc
etc
lib
plugins (v.0.2+)
src
share/
     licenses (v.0.2+)
     bouts
     skeletons
     hits
```

### Local service directories

Located at the same directory with `build.jab` file

```
.jabs/
     etc
     share/
           licenses (v.0.2+, optional)
           bouts (optional)
           skelethons (optional)
           hits (optional)
```

* `licenses` - 
* `bouts` - 
* `skelethons` - 
* `hits` - 

## Configfiles

### Global configfiles

```
PREFIX/etc/jabs.conf
PREFIX/etc/alias.conf (v.0.2+)
```

## Local project configfiles

Local project configs overrides global and located in service directory `.jabs` in the root of your. The hierarchy is:
```
.jabs/
      etc/
          alias.conf
          jabs.conf
          variables.conf
```

* `alias.conf` - Set of short CLI options as aliases for long JABS options for jab binary
* `jabs.conf` - Set of options for jabs (overrides by CLI options)
* `variables.conf` - Set of variables and its values for JABS preprocessor
