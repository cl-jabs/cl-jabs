# cl-jabs

![](https://github.com/cl-jabs/cl-jabs/blob/master/share/images/JABS_64x64.png) [![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE) [![Travis Build Status](https://api.travis-ci.org/cl-jabs/cl-jabs.svg?branch=master)](https://travis-ci.org/cl-jabs/cl-jabs) [![Join the chat at https://gitter.im/cl-jabs](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/cl-jabs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
```lisp
(Just
  (Another
    (Build
      (System :for "Common Lisp"))))
```

## WAT
![](https://media.giphy.com/media/1L5YuA6wpKkNO/giphy.gif)

JABS is a project of modern build system for common lisp. Code is under active development and still not ready for production usage. Please, find some documentation [here](https://github.com/cl-jabs/cl-jabs/wiki).

## [Quickstart](https://github.com/cl-jabs/cl-jabs/wiki/quickstart)

### Install
1. Fetch from repo:
 ```bash
 git clone https://github.com/cl-jabs/cl-jabs.git
 ```

2. Bootstrap:
 ```bash
 cd cl-jabs && ./promoter build
```

3. Install system wild (optionally):
 ```bash
 sudo ./promoter install [--prefix=/usr/local]
 ```
    **NOTE:** `jab` command is your friend

### Generate and prepare project

So, We are ready to go

1. Create your project and cd there:
 ```bash
 mkdir /path/to/your/source/code/my_project_name
 # ...
 # create files, write a code, use git, 
 # do something, like you're developer
 # ...
 cd /path/to/your/source/code/my_project_name
 ```

2. Create project buildfile template
 ```bash
 jab --autogen-project
 ```

3. Open generated `build.jab` file and edit it at your own customs (fill free to use asdf:defsystem syntax)

### Run project

1. Go go go!

 ```bash
 jab
 ```

### See Also

[Build and deployment process](Build-and-deployment-process)

## Requirements

* sbcl >= 1.0.52 (only SBCL supported now)

## License & Autor
| | |
|:--|---|
| **License** | [MIT](./LICENSE) |
| **Copyright** | (C) Alexander Vynnyk, 2015-2017 |
| **Author** | [Alexander Vynnyk](https://github.com/cosmonaut.ok) (<cosmonaut.ok@zoho.com>) |
