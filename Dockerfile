
FROM ocaml/opam2:4.07

# some of the following apt packages are likely already installed
RUN sudo apt-get install -y git make
RUN sudo apt-get install -y curl
RUN sudo apt-get install -y gcc
RUN sudo apt-get install -y bzip2
RUN sudo apt-get install -y wget
RUN sudo apt-get install -y unzip m4
RUN sudo apt-get install -y time
RUN sudo apt-get install -y rsync bubblewrap

RUN opam update

# install some common packages, so they are cached in future docker builds
RUN opam install dune ocamlfind odoc
RUN opam install core_kernel 
RUN opam install core
RUN opam install re

# drop the RUN prefix from the following lines (and ignore previous lines!)
# to build using local opam install

RUN opam pin -y -n add tjr_lib_core https://github.com/tomjridge/tjr_lib.git
RUN opam pin -y -n add tjr_lib https://github.com/tomjridge/tjr_lib.git
RUN opam pin -y -n add tjr_profile https://github.com/tomjridge/tjr_profile.git
RUN opam pin -y -n add tjr_monad https://github.com/tomjridge/tjr_monad.git
RUN opam pin -y -n add tjr_fs_shared https://github.com/tomjridge/tjr_fs_shared.git
RUN opam pin -y -n add isa_btree https://github.com/tomjridge/isa_btree.git
RUN opam pin -y -n add isa_btree_test https://github.com/tomjridge/isa_btree.git
RUN opam pin -y -n add tjr_btree https://github.com/tomjridge/tjr_btree.git#dev
RUN opam pin -y -n add tjr_btree_examples https://github.com/tomjridge/tjr_btree.git#dev
RUN opam pin -y -n add tjr_lru_cache https://github.com/tomjridge/tjr_lru_cache.git
RUN opam pin -y -n add tjr_mem_queue https://github.com/tomjridge/tjr_mem_queue.git
RUN opam pin -y -n add tjr_pcache https://github.com/tomjridge/tjr_pcache.git
RUN opam pin -y -n add tjr_pcache_example https://github.com/tomjridge/tjr_pcache.git
RUN opam pin -y -n add tjr_pcache_test https://github.com/tomjridge/tjr_pcache.git
RUN opam pin -y -n add tjr_kv https://github.com/tomjridge/tjr_kv.git

RUN opam install -y tjr_btree

# FIXME also tjr_btree_examples tjr_pcache_example