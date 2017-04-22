set -a # export all vars
# set -x # debug

libname=tjr_btree

root=$(realpath $(dirname $BASH_SOURCE))/../..

 # if using nix, this may not be present
test -f $root/config.sh && source $root/config.sh

PKGS="-package num,yojson,ppx_deriving_yojson,batteries,extunix,extlib"

SYNTAX="" # "-syntax camlp4o" # simplify: use for every file
FLGS="-g -thread"

    # 8~"pattern-matching is not exhaustive"; 
    # 11~"this match case is unused";
    # 26~"unused variable s2"
    # 40~It is not visible in the current scope, and will not be selected if the type becomes unknown.
WARN="-w @f@p@u@s@40-8-11-26-40"

    # these include syntax, so should work on all files; may be
    # overridden in ocamlc.sh
  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   $FLGS $WARN $PKGS $SYNTAX"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt $FLGS $WARN $PKGS $SYNTAX"
ocamldep="ocamlfind ocamldep $PKGS"

mk_cma="$DISABLE_BYTE ocamlfind ocamlc $FLGS "
mk_cmxa="$DISABLE_NTVE ocamlfind ocamlopt $FLGS"

# gen_isa.ml 

# mls ----------------------------------------

# remove some files using sed
mls=`cat _depend/* | sed 's/gen_our.ml//g'`

cmos="${mls//.ml/.cmo}"
cmxs="${mls//.ml/.cmx}"

natives="test_main.native main.native"

bytes="test_main.byte"



# depend ----------------------------------------

function mk_depend() {
    mkdir -p _depend
    for f in {b,c,d,e,f,g,h,i,j,m}_*; do
    # for f in {b,c}_* d_core; do
        (cd $f && ocamldep -one-line -sort *.ml > ../_depend/$f)
    done
}



# links ----------------------------------------

function init() {
    link_files=`ls {b,c,d,e,f,g,h,i,j,m}_*/*.ml`
}

function mk_links() {
    echo "mk_links..."
    init
    ln -s $link_files .
}


function rm_links() {
    echo "rm_links..."
    init
    for f in $link_files; do rm -f `basename $f`; done
}


# mlis ----------------------------------------

function mk_mlis() {
    echo "mk_mlis..."
    for f in $mls; do $ocamlc -i $f > tmp/${f/.ml/.mli}; done
}



# meta ----------------------------------------

function mk_meta() {
local gv=`git rev-parse HEAD`
local d=`date`
cat >META <<EOF
name="$libname"
description="Pure functional B-tree implementation"
version="$d $gv"
depends=""
archive(byte)="$libname.cma"
archive(native)="$libname.cmxa"
EOF

}


# codetags ----------------------------------------

function mk_codetags() {
    init # link_files
    for f in XXX TODO FIXME NOTE QQQ; do     # order of severity
        grep --line-number $f $link_files || true; 
    done
}


# ocamlfind install, remove, reinstall --------------------

function install() {
	  ocamlfind install $libname META `find . -name "*.cmi" -o -name "*.cma" -o -name "*.cmxa" -o -name "*.a"`
}

function remove() {
    ocamlfind remove $libname
}


# # new_bak ----------------------------------------
# 
# function new_bak() {
#     local n=1 
#     while [ -f "$1.bak.$n" ]; do ((++n)); done
#     echo "$1.bak.$n"
# }

