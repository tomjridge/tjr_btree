DUNE:=dune

build:
	$(DUNE) build @install
	$(DUNE) build examples/all.touch
	$(DUNE) build test_bin/all.touch

install:
	$(DUNE) install

uninstall:
	$(DUNE) uninstall

clean:
	$(DUNE) clean
	rm -f examples/btree.store
	rm -f test_bin/btree.store

doc: FORCE
	$(DUNE) build @doc

doc_install: doc
	rm -rf ocamldoc/*
	cp -R _build/default/_doc/_html/* ocamldoc

run_examples:
	$(MAKE) -C examples -f Makefile.run_examples

run_tests:
	$(MAKE) -C test_bin -f Makefile.run_tests

FORCE:


# 
# clean:
# 	$(MAKE) -C src real_clean
# 	$(MAKE) -C examples clean
# 	$(MAKE) -C test_bin clean

