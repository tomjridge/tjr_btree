# NOTE maybe add a "RUN echo ..." to force a rebuild, or --no-cache

# FROM ocaml/opam:ubuntu - latest doesn't work due to problems with ppx_deriving deps
# 17.04 doesn't work due to missing archives at ubuntu? or stale packages info?
FROM ocaml/opam:ubuntu-16.04_ocaml-4.04.2
# RUN apt-get -y update

RUN opam list
RUN opam update
RUN opam install batteries extunix
RUN opam install ppx_deriving_yojson extlib core ppx_bin_prot ocaml-compiler-libs lwt
RUN opam list
RUN ocamlfind list


RUN git clone https://github.com/tomjridge/isa_btree.git 
RUN eval `opam config env` && make -C isa_btree

RUN git clone https://github.com/tomjridge/tjr_monad.git 
RUN eval `opam config env` && make -C tjr_monad

RUN git clone https://github.com/tomjridge/tjr_fs_shared.git 
RUN eval `opam config env` && make -C tjr_fs_shared

RUN git clone -b dev https://github.com/tomjridge/tjr_btree.git
RUN eval `opam config env` && make -C tjr_btree 


# run some examples and tests
RUN eval `opam config env` && make -C tjr_btree/test_bin all run_tests 
RUN eval `opam config env` && make -C tjr_btree/examples all run_examples




# docker build -t some_name .
# docker run -ti some_name bash
