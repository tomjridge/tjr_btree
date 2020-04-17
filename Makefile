default: all

-include Makefile.ocaml

# for emacs completion
promote_docs::

# for auto-completion of Makefile target
clean::
	rm -f example.store

cln_stores:
	rm -f *.store

run:
	$(DUNE) exec bin/btree_main.exe example # >tmp.txt 2>&1

run_test: # FIXME todo
	dune exec src-test/test.exe
