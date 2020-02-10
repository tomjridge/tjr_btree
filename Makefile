TMP_DOC_DIR:=/tmp/tjr_btree
scratch:=/tmp/l/github/scratch

default: all

-include Makefile.ocaml

# for emacs completion
promote_docs::

# for auto-completion of Makefile target
clean::
	rm -f btree.store

run_eg1:
	$(DUNE) exec bin/btree_main.exe eg1

run_examples: # FIXME todo
	@echo ======================================================================
	@echo
	btree_main eg1 |sed 's/^/    /'
	@echo 
	@echo ======================================================================
	@echo
	./src-examples/demo_int_int_map.sh |sed 's/^/    /'
	@echo 
	@echo ======================================================================
#	$(MAKE) -f src-examples/Makefile.run_examples

# run_tests: # FIXME todo
# 	$(MAKE) -C test_bin -f Makefile.run_tests

run_test:
	dune exec src-test/test.exe
