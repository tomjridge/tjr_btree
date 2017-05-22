set -a # export all vars
# set -x # debug

libname=tjr_btree
Libname=Tjr_btree
src_subdirs=`echo {ad,ag,b,c,d,e,f,g,h,i,j,n}_*`
mls_in_subdirs=`ls {ad,ag,b,c,d,e,f,g,h,i,j,n}_*/*.ml`
meta_description="A CoW B-tree library"

PKGS1="num,yojson,ppx_deriving_yojson,batteries,extunix,extlib"
PKGS="-package num,yojson,ppx_deriving_yojson,batteries,extunix,extlib"

source bash_env.common

natives="main.native test_main.native simple_example.native"

bytes="test_main.byte"


