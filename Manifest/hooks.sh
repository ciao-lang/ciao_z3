#!/bin/bash

# Exit immediately if a simple command exits with a non-zero status
set -e

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

old_dir=`pwd`; cd "$_base/.."; bdlroot=`pwd`; cd "$old_dir"; old_dir=

# ---------------------------------------------------------------------------
# Configuration

# NOTE: Look at https://github.com/Z3Prover/z3/releases for releases
# (there are 32-bit versions too)

z3_name="z3-4.4.1"

function select_bin_dist() {
    z3_baseurl="https://github.com/Z3Prover/z3/releases/download/$z3_name"
    case "$CIAO_OS" in
	LINUX)
	    z3_file="$z3_name""-x64-ubuntu-14.04.zip" ;;
	DARWIN)
	    z3_file="$z3_name""-x64-osx-10.11.zip" ;;
	*)
	    echo "ERROR: Unsupported CIAO_OS=$CIAO_OS" 1>&2
	    exit 1
    esac
}

function select_src_dist() {
    z3_baseurl="https://github.com/Z3Prover/z3/archive"
    z3_file="z3-4.4.1-src.tar.gz"
}

# --------------------------------------------------------------------------

if [ "$THIRDPARTY" = "" ]; then
    cat <<EOF
ERROR: THIRDPARTY directory missing (use 'ciao build')
EOF
    exit 1
fi

cachedir="$THIRDPARTY/cache"
storedir="$THIRDPARTY/store"
srcdir="$THIRDPARTY/src"

# --------------------------------------------------------------------------

# Uncompress the archive, striping first path component
# handle both .zip and .tar.gz
function uncompress_strip1() { # source target
    case $1 in
	*.tgz|*.tar.gz) uncompress_tgz "$1" "$2" ;;
	*.zip) uncompress_zip "$1" "$2" ;;
    esac
}

function uncompress_zip() { # source target
    local temp=`mktemp -d`
    unzip -q -d "$temp" "$1"
    mkdir -p "$2"
    local f=("$temp"/*) # (array init)
    if (( ${#f[@]} == 1 )) && [[ -d "${f[0]}" ]] ; then	# one element
        mv "$temp"/*/* "$2"
    else # more than one element
        mv "$temp"/* "$2"
    fi
    rmdir "$temp"/* "$temp"
}

function uncompress_tgz() { # source target
    tar -xz --strip-components 1 -f "$1" -C "$2"
}

# --------------------------------------------------------------------------

function fetch_z3() {
    # Ensure that cachedir is created
    mkdir -p "$cachedir"

    # Download z3
    rm -f "$cachedir/$z3_file"
    z3_url="$z3_baseurl/$z3_file"
    curl -L "$z3_url" -o "$cachedir/$z3_file"
}

function uncompress_z3_bin() {
    # Cleanup storedir for z3 and uncompress
    rm -rf "$storedir/$z3_name"
    mkdir -p "$storedir/$z3_name"
    uncompress_strip1 "$cachedir/$z3_file" "$storedir/$z3_name"
    # Fix missing lib/ (point to bin)
    ln -s bin "$storedir/$z3_name/lib"
}

function uncompress_z3_src() {
    # Cleanup srcdir for z3 and uncompress
    rm -rf "$srcdir/$z3_name"
    mkdir -p "$srcdir/$z3_name"
    uncompress_strip1 "$cachedir/$z3_file" "$srcdir/$z3_name"
}

# TODO: (Not tested)
function build_z3() {
    pushd "$srcdir/$z3_name" > /dev/null 2>&1

    # LDFLAGS="-L$THIRDPARTY/lib" CPPFLAGS="-I$THIRDPARTY/include" LD_LIBRARY_PATH="$THIRDPARTY/lib" ./configure
    python scripts/mk_make.py --prefix="$storedir/$z3_name"
    cd build
    make
    make install
    
    popd > /dev/null 2>&1
}

function fix_dylibs() {
    # NOTE: (no lib name with version)
    local z3libVerN
#    local z3libN
    case "$CIAO_OS" in
	LINUX)
	    z3libVerN="libz3.so"
#	    z3libN="libz3.so"
	    ;;
	DARWIN)
	    z3libVerN="libz3.dylib"
#	    z3libN="libz3.dylib"
	    ;;
    esac
    local z3libVer="$storedir/$z3_name/lib/$z3libVerN"
#    local z3lib="$storedir/$z3_name/lib/$z3libN"
    # Fix install dir (it was /usr/local)
    case "$CIAO_OS" in
	LINUX)
	    pushd "$storedir/$z3_name" > /dev/null 2>&1
	    /sbin/ldconfig -n "lib"
#            # Link name without version
#	    ln -sf "$z3libVerN" "lib/$z3libN"
	    popd > /dev/null 2>&1
	    ;;
	DARWIN)
	    install_name_tool -id "$z3libVer" "$z3libVer"
#            # Link name without version
#	    ln -sf "$z3libVer" "$z3lib"
	    ;;
    esac
}

# ---------------------------------------------------------------------------

function gen_config_auto() {
    local RPATH=
    case "$CIAO_OS" in
	LINUX)
	    RPATH="-Wl,-rpath,$storedir/$z3_name/lib,-rpath,\\'\$ORIGIN\\'"
	    ;;
    esac
    cat > $bdlroot/src/ciao_z3_config_auto.pl <<EOF
:- extra_compiler_opts([
	% For Z3
	'-I$storedir/$z3_name/include'
	]).
:- extra_linker_opts(' -L.').
:- extra_linker_opts([
	% For Z3
	'$RPATH -L$storedir/$z3_name/lib'
	]).

:- use_foreign_library(['z3']).
EOF
}

# ===========================================================================

function install_dist() { # Mode=bin|src
    if [ -x "$storedir/$z3_name" ]; then
	# echo "z3 already downloaded" 1>&2
	return 0
    fi

    if [ "$1" = bin ]; then
	select_bin_dist
    else # src
	select_src_dist
    fi
    fetch_z3
    if [ "$1" = bin ]; then
	uncompress_z3_bin
    else # src
	uncompress_z3_src
	build_z3
    fi
    fix_dylibs
}

# ===========================================================================

case $1 in
    install_bin_dist) install_dist bin ;;
    install_src_dist) install_dist src ;;
    gen_conf) gen_config_auto ;;
    *)
	echo "ERROR: Unknown action" 1>&2
	exit 1
esac
