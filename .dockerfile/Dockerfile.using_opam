FROM impfs_base_docker_image

RUN echo build from here .......

RUN eval `opam config env` && opam pin -y -n add tjr_lib_core https://github.com/tomjridge/tjr_lib.git 
RUN eval `opam config env` && opam pin -y -n add tjr_lib https://github.com/tomjridge/tjr_lib.git 
RUN eval `opam config env` && opam pin -y -n add isa_btree https://github.com/tomjridge/isa_btree.git 
RUN eval `opam config env` && opam pin -y -n add tjr_monad https://github.com/tomjridge/tjr_monad.git 
RUN eval `opam config env` && opam pin -y -n add tjr_fs_shared https://github.com/tomjridge/tjr_fs_shared.git 
RUN eval `opam config env` && opam pin -y -n add tjr_btree https://github.com/tomjridge/tjr_btree.git#dev
RUN eval `opam config env` && opam install -y tjr_btree
