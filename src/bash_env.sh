set -a # export all vars
# set -x # debug

libname=tjr_btree
Libname=Tjr_btree
src_subdirs=`echo {ac,ad,ag,b,c,d,e,f,g,h,i,j,n}_*`
mls_in_subdirs=`ls {ac,ad,ag,b,c,d,e,f,g,h,i,j,n}_*/*.ml`
meta_description="A CoW B-tree library"

required_packages="num,yojson,ppx_deriving_yojson,batteries,extunix,extlib,core,ppx_bin_prot"

natives="main.native test_main.native simple_example.native ii_example.native"
bytes="test_main.byte"

source bash_env.common



