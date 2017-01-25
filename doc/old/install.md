# Installation

## Getting repository
* from git <code>git clone https://github.com/cosmonaut-ok/cl-jabs.git</code>

## Preparing

```
cd /where/you/cloned/your/cl-jabs/
user@host cl-jabs $ ./bootstrap.sh
```

## Install
a. **install it globally**
```
root@host cl-jabs # ./jab --self-install --prefix=/usr/local
```

b. **install it locally**
```
user@host cl-jabs $ ./jab --self-install --prefix=~/bin
```

c. of course, you can use jab from current directory without installation

Also, you can set `--bindir`, `--libdir`, `--sysconfigdir`, `--datarootdir`, `--docdir` and `--mandir` during installation
