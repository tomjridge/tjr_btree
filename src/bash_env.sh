set -a # export all vars
# set -x # debug

libname=tjr_btree
Libname=Tjr_btree

# NOTE r_recap is for doc only
src_subdirs=`echo {ac,ag,c,d,e,f,h,i,j,n}_*`
mls_in_subdirs=`ls {ac,ag,c,d,e,f,h,i,j,n}_*/*.ml`
meta_description="A CoW B-tree library"

required_packages="num,yojson,ppx_deriving_yojson,batteries,extunix,extlib,core,ppx_bin_prot,ocaml-compiler-libs,tjr_fs_shared"

natives="main.native test_main.native simple_example.native ii_example.native"
bytes="test_main.byte"





# generic ---------------------------------------------------------------

# set these env vars before including the file
function check_env_vars () {
    # http://stackoverflow.com/questions/31164284/shell-script-exiting-script-if-variable-is-null-or-empty
    : ${libname?Need a value}
    : ${Libname?Need a value}
    : ${src_subdirs?Need a value}
    : ${mls_in_subdirs?Need a value}
    : ${meta_description?Need a value}
    : ${required_packages?Need a value}
}
check_env_vars

PKGS="-package $required_packages"


# root=$(realpath $(dirname $BASH_SOURCE))/../..
# 
#  # if using nix, this may not be present
# test -f $root/config.sh && source $root/config.sh


SYNTAX="" # "-syntax camlp4o" # simplify: use for every file
FLGS="-g -thread -bin-annot" 
INLINE="-inline 0" # inline 0 for debugging native
FOR_PACK="-for-pack $Libname"

    # 8~"pattern-matching is not exhaustive"; 
    # 11~"this match case is unused";
    # 26~"unused variable s2"
    # 40~It is not visible in the current scope, and will not be selected if the type becomes unknown.
WARN="-w @f@p@u@s@40-8-11-26-40"

    # these include syntax, so should work on all files; may be
    # overridden in ocamlc.sh; FIXME don't need -bin-annot twice
  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   -bin-annot         $FLGS $FOR_PACK $WARN $PKGS $SYNTAX"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt -bin-annot $INLINE $FLGS $FOR_PACK $WARN $PKGS $SYNTAX"
ocamldep="ocamlfind ocamldep $PKGS"


# mls ----------------------------------------

mls=`test -e _depend/xxx && cat _depend/*`

cmos="${mls//.ml/.cmo}"
cmxs="${mls//.ml/.cmx}"


# cma,cmxa -------------------------------------------------------------

function mk_cma() {
         # NOTE -bin-annot
	$DISABLE_BYTE ocamlfind ocamlc -bin-annot -pack -o $libname.cmo $cmos
  $DISABLE_BYTE ocamlfind ocamlc -g -a -o $libname.cma $libname.cmo
}

function mk_cmxa() {
	$DISABLE_NTVE ocamlfind ocamlopt -pack -o $libname.cmx $cmxs
  $DISABLE_NTVE ocamlfind ocamlopt -g -a -o $libname.cmxa $libname.cmx
}


# depend ----------------------------------------

function mk_depend() {
    mkdir -p _depend
    for f in ${src_subdirs}; do
        (cd $f && ocamldep -one-line -sort *.ml > ../_depend/$f)
    done
    touch _depend/xxx
}



# links ----------------------------------------

function init() {
    link_files="${mls_in_subdirs}"
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
    for f in $mls_in_subdirs; do $ocamlc -i $f > tmp/${f/.ml/.mli}; done
}



# meta ----------------------------------------

function mk_meta() {
local gv=`git rev-parse HEAD`
local d=`date`
cat >META <<EOF
name="$libname"
description="$meta_description"
version="$d $gv"
requires="$required_packages"
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


# doc ----------------------------------------------------

function mk_doc() {
    ocamlfind ocamldoc $PKGS $WARN -html -intro n_doc/intro.odoc `cat _depend/*`
    # FIXME assume package built and installed
    # $ocamlc -package tjr_btree -i tjr_btree_recap.ml >tjr_btree_recap.mli
    echo NOTE that there are also recap docs in r_recap/
}


# clean ----------------------------------------------------------------

function clean() {
	rm -f *.{cmi,cmo,cmx,o,cmt} a.out *.cma *.cmxa *.a *.byte *.native
}

# ocamlfind install, remove, reinstall --------------------

function install() {
    # assumes packing
	  ocamlfind install $libname META $libname.{cmi,cmo,cma,cmx,cmxa,a,cmt} *.cmt *.ml
    # FIXME how to install cmt file for tjr_btree?
}

function remove() {
    ocamlfind remove $libname
}
