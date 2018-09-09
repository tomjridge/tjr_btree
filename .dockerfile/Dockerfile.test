FROM tjr_btree_from_github

# run some examples and tests ------------------------------------------

RUN eval `opam config env` && make -C tjr_btree/test_bin all run_tests 
RUN eval `opam config env` && make -C tjr_btree/examples all run_examples
