FROM impfs_base_docker_image

RUN echo build from here ....

RUN git clone https://github.com/tomjridge/tjr_lib.git 
RUN eval `opam config env` && make -C tjr_lib && make -C tjr_lib install


RUN git clone https://github.com/tomjridge/isa_btree.git 
RUN eval `opam config env` && make -C isa_btree

RUN git clone https://github.com/tomjridge/tjr_monad.git 
RUN eval `opam config env` && make -C tjr_monad

RUN git clone https://github.com/tomjridge/tjr_fs_shared.git 
RUN eval `opam config env` && make -C tjr_fs_shared


# from github ----------------------------------------------------------

RUN git clone -b dev https://github.com/tomjridge/tjr_btree.git
RUN eval `opam config env` && make -C tjr_btree 


# run some examples and tests ------------------------------------------

RUN eval `opam config env` && make -C tjr_btree/test_bin all run_tests 
RUN eval `opam config env` && make -C tjr_btree/examples all run_examples
