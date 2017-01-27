#!/bin/sh -x

set -e

ROOT=$(dirname $(dirname $(realpath $0)))
DIR=`mktemp -d /tmp/cl-jabs-XXXX`

cd ${DIR}

echo '(in-package :jabs)' > testfile.lisp
echo '(format nil "Hello World~%")' >> testfile.lisp

${ROOT}/src/wrappers/jn/jn.sh -Dbouts=tmpl-mkproject -Dplugins=tmpl@generic -Dtmpl-skelethon=flat
${ROOT}/src/wrappers/jn/jn.sh -Dprojects=$(basename ${DIR})

rm -rf $DIR
