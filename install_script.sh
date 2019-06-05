#!/bin/bash

opam install -y dune ocamlfind odoc
opam pin add -y -n tjr_lib_core https://github.com/tomjridge/tjr_lib.git
opam pin add -y -n tjr_lib https://github.com/tomjridge/tjr_lib.git
opam pin add -y -n tjr_fs_shared https://github.com/tomjridge/tjr_fs_shared.git
opam pin add -y -n tjr_monad https://github.com/tomjridge/tjr_monad.git
opam pin add -y -n tjr_profile https://github.com/tomjridge/tjr_profile.git
opam pin add -y -n isa_btree https://github.com/tomjridge/isa_btree.git
opam pin add -y -n tjr_btree https://github.com/tomjridge/tjr_btree.git#dev
opam pin add -y -n tjr_btree_examples https://github.com/tomjridge/tjr_btree.git#dev
opam install -y tjr_btree_examples
