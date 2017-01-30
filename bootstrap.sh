#!/bin/sh

## colors
if which tput > /dev/null 2>&1 && [ $(tput -T$TERM colors 2>/dev/null || echo 0) -ge 8 ]; then
    ## action: PURPLE, name: CYAN, passed: BLUE, failed: RED, step done GREEN
    GRAY="\[\033[1;30m\]"
    BLUE="\033[0;34m"
    CYAN="\033[0;36m"
    PURPLE="\033[0;35m"
    GREEN="\033[0;32m"
    RED="\033[0;31m"
    YELLOW="\033[0;33m"
    WHITE="\033[0;37m"
    NO_COLOUR="\033[0m"
fi
## /colors

BASEDIR=$PWD
LIBS="asdf"
# LIBS=""
SBCL_BIN=$(which sbcl)
SBCL_URI="http://downloads.sourceforge.net/project/sbcl/sbcl/1.3.4/sbcl-1.3.4-x86-64-linux-binary.tar.bz2?r=http%3A%2F%2Fwww.sbcl.org%2Fplatform-table.html&ts=1461060178&use_mirror=tenet"
PATH=$PATH:$BASEDIR/tmp/bin

print_help()
{
    echo "This is bootstrap script for ${CYAN}(${RED}J${NO_COLOUR}ust ${CYAN}(${RED}A${NO_COLOUR}nother ${CYAN}(${RED}B${NO_COLOUR}uild ${CYAN}(${RED}S${NO_COLOUR}ystem${CYAN}))))${NO_COLOUR}"
    echo "JABS version: $(grep -R 'defconstant +jabs-version+' src/jabs-core.lisp.input | cut -d \" -f2)"
    echo
    echo "${PURPLE}Usage: ${CYAN}$0${NO_COLOUR} ${GREEN}[help|clean|test|install]${NO_COLOUR}"
    echo

}

check_sbcl()
{
    echo -n ">> ${PURPLE}Checking${NO_COLOUR} for ${CYAN}sbcl${NO_COLOUR} binary present in system... "
    test -z $SBCL_BIN && echo "${CYAN}sbcl${NO_COLOUR} binary not found in system" && return 5
    test -z "$SBCL" && SBCL="$SBCL_BIN --noinform --disable-ldb --disable-debugger --no-sysinit --no-userinit"
    echo "${PURPLE}DONE${NO_COLOUR}"
}

do_sbcl()
{
    echo -n ">> ${PURPLE}Installing${NO_COLOUR} ${CYAN}sbcl${NO_COLOUR} locally... "
    mkdir -p tmp
    [ ! $? -eq 0 ] && echo "Can not make tmp directory. Exiting" && return 1
    cd tmp
    mkdir -p bin
    wget -q $SBCL_URI -O sbcl.tar.gz
    tar -xf sbcl.tar.gz
    ln -sf $BASEDIR/tmp/sbcl-*/run-sbcl.sh $BASEDIR/tmp/bin/sbcl
    cd $BASEDIR
    SBCL_BIN=$(which sbcl)
    echo "${PURPLE}DONE${NO_COLOUR}"
}

do_unittest()
{
    local name=$(basename $1 .lisp)
    echo "${PURPLE}Running${NO_COLOUR} test module ${CYAN}${name}${NO_COLOUR}"
    $SBCL --eval "(require 'asdf)" --script test/unit/$1
    if [ "$?" != 0 ] ; then
        echo "Test module ${CYAN}${name} ${RED}FAILED${NO_COLOUR}">&2
        return 1
    fi
    echo "Test module ${CYAN}${name} ${BLUE}PASSED${NO_COLOUR}"
}

do_install()
{
    local DSTDIR=$1
    local J_PREFIX=$DSTDIR
    local J_BINDIR=$DSTDIR/bin
    local J_LIBDIR=$DSTDIR/lib
    local J_SYSCONFDIR=$DSTDIR/etc/
    local J_DATAROOTDIR=$DSTDIR/share
    local J_DOCDIR=$DSTDIR/doc
    local J_MANDIR=$DSTDIR/man
    ##
    mkdir -p $DSTDIR
    if [ -x ./jab ]; then
        ./jab --self-install --prefix $J_PREFIX --bindir $J_BINDIR --libdir $J_LIBDIR --sysconfigdir $J_SYSCONFDIR --datarootdir $J_DATAROOTDIR --docdir $J_DOCDIR --mandir $J_MANDIR >/dev/null
    else
        echo "There is no ${CYAN}./jab${NO_COLOUR} file. Installation ${RED}failed${NO_COLOUR}">&2
        return 1
    fi
}

do_clean()
{
    cd $BASEDIR
    echo -n "${PURPLE}Clearing${NO_COLOUR} working directory... "
    rm -f jab src/wrappers/jab/jab.lisp src/jabs-loader.lisp src/jabs-core.lisp
    rm -rf tmp
    rm -f `find . -name '*~'`
    ## resetting modules state (could change state during build)
    rm -rf lib/*
    git submodule update --init >/dev/null 2>&1
    echo "${PURPLE}DONE${NO_COLOUR}"
}


do_build()
{
    [ -z "$(which git)" ] || [ ! -d .git ] && echo "There is ${RED}no ${CYAN}git${NO_COLOUR} executable or ${RED}not in git repository${NO_COLOUR}" && return 1

    git submodule update --init
    ## preparing ASDF to use new texinfo
    sed -i 's/\&rest/Arest/g;s/\&key/Akey/g;s/\&optional/Aoptional/g;s/\&allow-other-keys/Aallow-other-keys/g;s/\&body/Abody/g' lib/asdf/doc/asdf.texinfo
    ## building libraries
    if [ -n "$LIBS" ]; then
        for i in "$LIBS"; do
            cd lib/$i
            make
            local ret=$?
            cd $BASEDIR
            [ $ret != 0 ] && echo "${PURPLE}Build${NO_COLOUR} library ${CYAN}$i ${RED}FAILED${NO_COLOUR}">&2 && return $ret
        done
    fi
    if [ -f lib/asdf/build/asdf.lisp ]; then
        cp lib/asdf/build/asdf.lisp src/core/jabs-asdf/
    else
        echo "${PURPLE}Build${NO_COLOUR} ASDF library ${RED}FAILED${NO_COLOUR}. There is no file ${CYAN}lib/asdf/build/asdf.lisp${NO_COLOUR}">&2
        return 1
    fi

    if [ -f lib/quicklisp-bootstrap/quicklisp.lisp ]; then
        cp lib/quicklisp-bootstrap/quicklisp.lisp share/plugins/repository/quicklisp/quicklisp.lisp
    else
        echo "${PURPLE}Build${NO_COLOUR} Quicklisp bootstrap library ${RED}FAILED${NO_COLOUR}. There is no file ${CYAN}share/plugins/repository/quicklisp/${NO_COLOUR}">&2
        return 1
    fi


    echo -n "${PURPLE}Generating${NO_COLOUR} files from templates..."
    sed "s|@SRCDIR@|$PWD/src/|" src/wrappers/jab/jab.lisp.input > src/wrappers/jab/jab.lisp
    sed "s|@SRCDIR@|$PWD/src/|" src/jabs-loader.lisp.input > src/jabs-loader.lisp
    sed "s|@LIBDIR@|$PWD/lib/|" src/jabs-packages.lisp.input > src/jabs-packages.lisp
    sed "s|@DATADIR@|$PWD/|;s|@SRCDIR@|$PWD/src/|;s|@LIBDIR@|$PWD/lib/|;s|@DATAROOTDIR@|$PWD/share/|;s|@SYSCONFIGDIR@|$PWD/etc/|" src/jabs-core.lisp.input > src/jabs-core.lisp
    echo "${PURPLE}DONE${NO_COLOUR}"

    # $SBCL --load ./src/wrappers/jab/make-jab.lisp >/dev/null
    local ret=$?
    [ $ret != 0 ] && echo "${PURPLE}Build${NO_COLOUR} ${RED}FAILED${NO_COLOUR}">&2 && return $ret

    # echo
    # echo "You can operate now with ${CYAN}\`\`jab''${NO_COLOUR} utility from local directory, or do ${CYAN}\`\`./jab --self-install''${NO_COLOUR} to install it globally"
    # echo
    echo -n "${PURPLE}Generating${NO_COLOUR} documentation..."
    echo "# JABS Tools" > doc/Tools.md
    grep -E '\(defun|\(defmacro|\(defvar' src/jabs-tools.lisp | grep -vE '^;' | sed 's|(defvar|var|g;s|(defmacro|macro|g;s|(defun|fun|g' | while read i; do printf "* \`$i\`\n\n"; done >> doc/tools.md
    echo "${PURPLE}DONE${NO_COLOUR}"
}

case $1 in
    "help"|"-h"|"--help")
        print_help
        ;;
    "clean")
        do_clean
        ;;
    "test")
        if [ x$2 = "x-l" ]; then
            for k in `ls test/unit`; do
                echo $(basename $k .lisp)
            done
            exit 0
        fi
        ## check for sbcl binary
        check_sbcl || do_sbcl
        ret=$?
        [ $ret != 0 ] && echo ">> ${RED}FAILED${NO_COLOUR} step">&2 && exit $ret
        echo ">> ${PURPLE}DONE${NO_COLOUR} step"
        echo
        ## building
        echo ">> ${PURPLE}Testing ${CYAN}build process${NO_COLOUR}"
        do_build>/dev/null
        [ $? != 0 ] && echo ">> ${RED}FAILED${NO_COLOUR} step">&2 && exit 1
        echo ">> ${PURPLE}DONE${NO_COLOUR} step"
        echo
        if [ ! -z $2 ]; then
            echo ">> ${PURPLE}Running${NO_COLOUR} test $2"
            do_unittest ${2}.lisp
            exit $?
        else
            ## unit tests
            echo ">> ${PURPLE}Running${NO_COLOUR} tests"
            for i in $(ls test/unit/*.lisp); do
                do_unittest $(basename $i) | grep -vE '^ok [0-9]*'
                [ $? != 0 ] && echo ">> ${RED}FAILED${NO_COLOUR} step">&2 && exit 1
            done
            echo ">> ${PURPLE}DONE${NO_COLOUR} step"
            echo
            ## install process
            echo ">> ${PURPLE}Testing ${CYAN}installation process${NO_COLOUR}"
            DSTDIR=`mktemp -d /tmp/.jabs.XXXXXX`
            do_install $DSTDIR
            ## testing if installation completed
            J_PREFIX=$DSTDIR
            J_BINDIR=$DSTDIR/bin
            J_LIBDIR=$DSTDIR/lib
            J_SYSCONFDIR=$DSTDIR/etc/
            J_DATAROOTDIR=$DSTDIR/share
            if [ -x $J_BINDIR/jab ] && \
                   [ -f $J_SYSCONFDIR/jabs/alias.conf ] && [ -f $J_SYSCONFDIR/jabs/jabs.conf ] && \
                   [ -f $J_LIBDIR/jabs/asdf/asdf.lisp ] && \
                   [ -f $J_DATAROOTDIR/jabs/src/jabs-loader.lisp ]; then
                echo "${CYAN}Installation process ${BLUE}PASSED${NO_COLOUR}"
            else
                echo "${RED}Installation incomplete. Install process failed${NO_COLOUR}">&2
                exit 1
            fi
            echo ">> ${PURPLE}DONE${NO_COLOUR} step"
            echo ">> ${PURPLE}Testing ${CYAN} functionality${NO_COLOUR}"
            mkdir -p $J_PREFIX/workdir
            cd $J_PREFIX/workdir
            mkdir src
            touch src/test.lisp
            $J_BINDIR/jab --generate-buildfile -q > /dev/null
            [ $? != 0 ] && echo ">> ${RED}FAILED${NO_COLOUR} step">&2 && exit 1
            $J_BINDIR/jab -q && [ -d "doc" ] && [ -d "test" ]
            [ $? != 0 ] && echo ">> ${RED}FAILED${NO_COLOUR} step">&2 && exit 1
            echo ">> ${PURPLE}DONE${NO_COLOUR} step"
            echo
            echo ">> ${PURPLE}Clearing${NO_COLOUR}"
            rm -rf $DSTDIR
            do_clean >/dev/null
            [ $? != 0 ] && exit 1
            echo ">> ${PURPLE}DONE${NO_COLOUR} step"
            echo
        fi
        ;;
    "install")
        shift
        check_sbcl || do_sbcl
        ret=$?
        [ $ret != 0 ] && echo ">> ${RED}FAILED${NO_COLOUR} step">&2 && exit $ret
        echo "${PURPLE}DONE${NO_COLOUR}"
        echo "${PURPLE}Building${NO_COLOUR} JABS..."
        do_build
        echo "${PURPLE}DONE${NO_COLOUR}"
        echo
        echo "${PURPLE}Installing${NO_COLOUR} JABS..."
        do_install $@
        [ $? != 0 ] && exit 1
        echo "${PURPLE}DONE${NO_COLOUR}"
        ;;
    "prepare")
	check_sbcl || do_sbcl
	;;
    *)
        check_sbcl || do_sbcl
        ret=$?
        [ $ret != 0 ] && echo "${RED}FAILED${NO_COLOUR}">&2 && exit $ret
        echo "${PURPLE}DONE${NO_COLOUR}"
        echo "${PURPLE}Building${NO_COLOUR} JABS..."
        do_build
        echo "${PURPLE}DONE${NO_COLOUR}"
        ;;
esac
