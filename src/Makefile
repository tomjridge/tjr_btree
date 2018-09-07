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
	mk_meta
	$(MAKE) install

links:
	mk_links
	touch $@

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

clean: FORCE
	clean
	rm -f *.html
	rm -f tjr_btree_recap.mli

real_clean: clean
	rm_links
	rm -rf links _depend

FORCE:


