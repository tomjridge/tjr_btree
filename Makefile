DUNE:=dune

build:
	$(DUNE) build @install
#	$(DUNE) build examples/*.exe
# FIXME	$(DUNE) build test_bin/all.touch

install:
	$(DUNE) install

uninstall:
	$(DUNE) uninstall

clean:
	$(DUNE) clean
	rm -f examples/btree.store
	rm -f test_bin/btree.store


SRC:=_build/default/_doc/_html
DST:=docs
docs: FORCE
	$(DUNE) build @doc
	rm -rf $(DST)/*
	cp -R $(SRC)/* $(DST)

view_doc:
	google-chrome  $(SRC)/index.html

run_examples:
	$(MAKE) -C examples -f Makefile.run_examples

run_tests:
	$(MAKE) -C test_bin -f Makefile.run_tests

FORCE:


