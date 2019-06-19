SHELL:=bash
DUNE:=dune

build:
	$(DUNE) build @install
#	$(DUNE) build examples/ocaml/btree_main.exe
# FIXME	$(DUNE) build test_bin/all.touch

install:
	$(DUNE) install

uninstall:
	$(DUNE) uninstall

clean:
	$(DUNE) clean
	rm -f examples/btree.store
	rm -f test_bin/btree.store

all:
	$(MAKE) clean
	$(MAKE) build
	$(MAKE) install
	$(MAKE) docs


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


view_doc:
	google-chrome  $(SRC)/index.html

run:
	$(DUNE) exec examples/ocaml/btree_main.exe

run_examples:
	@echo ======================================================================
	@echo
	btree_main int_int_map_example |sed 's/^/    /'
	@echo 
	@echo ======================================================================
	@echo
	./examples/demo_int_int_map.sh |sed 's/^/    /'
	@echo 
	@echo ======================================================================
#	$(MAKE) -f examples/Makefile.run_examples

run_tests:
	$(MAKE) -C test_bin -f Makefile.run_tests

FORCE:


