SHELL:=bash
DUNE:=dune

bin:=bin

build:
	$(DUNE) build @install
#	$(DUNE) build $(bin)/btree_main.exe
# FIXME	$(DUNE) build test_bin/all.touch

install:
	$(DUNE) install

uninstall:
	$(DUNE) uninstall

clean:
	$(DUNE) clean

#	rm -f src-examples/btree.store
#	rm -f test_bin/btree.store

all:
	$(MAKE) clean
	$(MAKE) build
	$(MAKE) install
	$(MAKE) docs
	$(MAKE) src-test/test.exe

SRC:=_build/default/_doc/_html
DST:=docs
DST2:=/tmp/tjr_btree
docs: FORCE
	$(DUNE) build @doc
	@if [ ! -z "$$PROMOTE_DOCS" ]; then rm -rf $(DST)/* ; cp -R $(SRC)/* $(DST); echo "docs built and promoted to docs/"; else \
	  rsync -vaz $(SRC)/* $(DST2); echo "docs built in $(DST2) but not promoted to docs/"; fi

promote_docs: FORCE
	PROMOTE_DOCS=true $(MAKE) docs

tjr_btree_doc: FORCE
	$(DUNE) build --only-packages tjr_btree @doc
	rsync -vaz $(SRC)/* $(DST2); echo "docs built in $(DST2) but not promoted to docs/"

examples_doc: FORCE
	$(DUNE) build --only-packages tjr_btree_examples @doc
	rsync -vaz $(SRC)/* $(DST2); echo "docs built in $(DST2) but not promoted to docs/"

run_eg1:
	$(DUNE) exec $(bin)/btree_main.exe eg1

run_examples: # FIXME todo
	@echo ======================================================================
	@echo
	btree_main int_int_map_example |sed 's/^/    /'
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

FORCE:


