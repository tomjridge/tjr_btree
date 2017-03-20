set -a # export all vars
# set -x # debug

function new_bak() {
    local n=1 
    while [ -f "$1.bak.$n" ]; do ((++n)); done
    echo "$1.bak.$n"
}

libname=tjr_btree

root=$(realpath $(dirname $BASH_SOURCE))/../..

 # if using nix, this may not be present
test -f $root/config.sh && source $root/config.sh

# ,bos.setup  ppx_assert,ppx_assert.runtime-lib,sexplib,core,lru-cache,\ ,core_kernel

PKGS="-package num,yojson,ppx_deriving_yojson,batteries,extunix"

# -package tjr_lib"

SYNTAX="" # "-syntax camlp4o" # simplify: use for every file
FLGS="-g -thread"

# 8~"pattern-matching is not exhaustive"; 
# 11~"this match case is unused";
# 26~"unused variable s2"
# 40~It is not visible in the current scope, and will not be selected if the type becomes unknown.
WARN="-w @f@p@u@s@40-8-11-26-40"

# these include syntax, so should work on all files; may be overridden in ocamlc.sh
  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   $FLGS $WARN $PKGS $SYNTAX"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt $FLGS $WARN $PKGS $SYNTAX"
ocamldep="ocamlfind ocamldep $PKGS"

mk_cma="$DISABLE_BYTE ocamlfind ocamlc $FLGS "
mk_cmxa="$DISABLE_NTVE ocamlfind ocamlopt $FLGS"

# gen_isa.ml 

# mls ----------------------------------------

#btree.ml 

# device.ml \

mls=" \
gen_isa.ml \
our.ml \
\
tjr_string.ml \
test.ml pickle.ml btree_api.ml btree_util.ml cache.ml \
\
btree.ml 
btree_simple.ml \
\
map_int_int.ml \
map_digest_int.ml map_digest_to_string_int.ml map_string_int.ml map_string_string_small.ml \
map_idx_blk.ml \
\
block_device.ml file_store.ml \
in_mem_store.ml bytestore.ml \
\
device.ml root.ml \
\
int_int_filestore.ml kv_store_small.ml \
\
exhaustive.ml test_bytestore.ml test_cache.ml test_in_mem.ml test_ii.ml test_string_int.ml \
"
# kv_store_small.ml
# 

# test_in_mem.ml  test_ii.ml

cmos="${mls//.ml/.cmo}"
cmxs="${mls//.ml/.cmx}"

natives="test_main.native main.native"

bytes="test_main.byte"





# links ----------------------------------------

function init() {
    link_files=`ls b_from_isa/*.ml c_pre/*.ml d_core/*.ml e_maps/*.ml e_stores/*.ml e_user/*.ml f_examples/*.ml f_test/*.ml g_main/*.ml`
}

function mk_links() {
    echo "mk_links..."
    init
    ln -s $link_files .
    touch _links
}


function rm_links() {
    echo "rm_links..."
    init
    rm -f _links
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


# ocamlfind install, remove, reinstall

function install() {
	  ocamlfind install $libname META `find . -name "*.cmi" -o -name "*.cma" -o -name "*.cmxa" -o -name "*.a"`
}

function remove() {
    ocamlfind remove $libname
}
