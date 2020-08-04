default: 
	$(MAKE) all
#	$(MAKE) exes FIXME re-enable

-include Makefile.ocaml

# for emacs completion
promote_docs::

# for auto-completion of Makefile target
clean::
	rm -f example.store

cln_stores:
	rm -f *.store


update_generated_doc::
	cd src && (ocamldoc_pyexpander btree_intf.ml)
	cd src && (ocamldoc_pyexpander make_6.ml)
	cd src && (ocamldoc_pyexpander summary.t.ml > summary.ml)

run_example: exes
	$(DUNE) exec bin/btree_main.exe example # >tmp.txt 2>&1

exes: bin/btree_main.exe

%.exe: FORCE
	dune build $@

# run_test: test_exes
# 	dune exec src-test/test.exe

FORCE:
