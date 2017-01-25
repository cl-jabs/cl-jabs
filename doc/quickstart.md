# Quickstart

## Installing
1. Fetch from repo:
```bash
git clone https://cosmonaut@bitbucket.org/cosmonauts_default_team/cl-jabs.git
```

2. Bootstrap:
 ```bash
 cd cl-jabs && ./bootstrap.sh
 ```

3. Install to system (optionally):
```bash
./jab --self-install [--prefix=/usr]
```
    NOTE: `jab` util is your friend

## Generating and preparing project

So, We are ready to go

1. Create project
```bash
cd /path/to/your/source/code
```

2. Create project template
```bash
jab tmpl-mkproject -Dplugins=tmpl@generic -Dtmpl-skelethon=flat
```

3. Open file `build.jab` and edit it at your own customs (fill free to use asdf:defsystem syntax)

4. Run project
```bash
jab
```
