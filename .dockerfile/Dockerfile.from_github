FROM impfs_base_docker_image

RUN echo build from here .......

RUN git clone https://github.com/tomjridge/tjr_lib.git 
RUN eval `opam config env` && make -C tjr_lib build && make -C tjr_lib install

RUN git clone https://github.com/tomjridge/isa_btree.git 
RUN eval `opam config env` && make -C isa_btree build && make -C isa_btree install

RUN git clone https://github.com/tomjridge/tjr_monad.git 
RUN eval `opam config env` && make -C tjr_monad build && make -C tjr_monad install

RUN git clone https://github.com/tomjridge/tjr_fs_shared.git 
RUN eval `opam config env` && make -C tjr_fs_shared build && make -C tjr_fs_shared install


# from github ----------------------------------------------------------

RUN git clone -b dev https://github.com/tomjridge/tjr_btree.git
RUN eval `opam config env` && make -C tjr_btree build && make -C tjr_btree install


