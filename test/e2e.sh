#!/bin/sh -x

set -e

ROOT=$(dirname $(dirname $(realpath $0)))
DIR=`mktemp -d /tmp/cl-jabs-XXXX`
FAIL="false"

cd ${DIR}

echo '(in-package :jabs)' > testfile.lisp
echo '(format *error-output* "Hello World~%")' >> testfile.lisp

${ROOT}/src/wrappers/jn/jn.sh -Dbouts=tmpl-mkproject -Dplugins=tmpl@generic -Dtmpl-skelethon=flat

set +e

${ROOT}/src/wrappers/jn/jn.sh -Dproject=$(basename ${DIR}) 2>test

set -e

[ `grep -c "Hello World" test` -eq 1 ] || FAIL="true"

[ "$FAIL" = "true" ] && cat test

rm -rf $DIR

if [ "$FAIL" = "true" ]; then
    exit 1
else
    exit 0
fi
