SHELL:=/bin/bash
BASH_ENV=bash_env.sh
export BASH_ENV

# shouldn't shell builtin source bash_env anyway?
libname:=$(shell source bash_env.sh && echo $${libname})#
# $(info libname is $(libname)!)

# ----------------------------------------

all: links _depend
	$$ocamlc -c $$mls
	$$ocamlopt -c $$mls
	mk_cma
	mk_cmxa
	$(MAKE) exhaustive_in_mem_main.native
#	$(MAKE) $$natives $$bytes



links:
	mk_links
	touch $@

test:
	OCAMLRUNPARAM=b ./test_main.native ./default_tests.json

generated: 
	$(MAKE) -C b_from_isa

mlis: 
	mk_mlis

install:
	mk_meta
	-remove
	install

remove: 
	remove

reinstall: 
	remove
	install

# README.html: README.adoc
# 	asciidoctor $<

_depend:
	mkdir -p _depend
	mk_depend

codetags:
	mk_codetags

doc:
	echo Make sure to build first
	mk_doc
	cp *.html *.css ../docs


# ----------------------------------------

%.native: m_main/%.ml $(libname).cmxa FORCE
	$$ocamlopt -g -linkpkg -open $$Libname -o $*.native $$libname.cmxa $<

%.byte: m_main/%.ml $(libname).cma FORCE
	$$ocamlc -g -linkpkg -open $$Libname -o $*.byte $$libname.cma $<

clean:
	clean
	rm -f *.html
	rm -f tjr_btree_recap.mli
	echo	# following was in real_clean
	rm_links
	rm -rf links _depend

FORCE:


