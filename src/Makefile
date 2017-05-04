SHELL:=/bin/bash

# bash will read this config file first
BASH_ENV=bash_env.sh
export BASH_ENV

# shouldn't shell builtin source bash_env anyway?
libname:=$(shell source bash_env.sh && echo $${libname})#

# $(info libname is $(libname)!)

# ----------------------------------------

all: _links _depend
	$$ocamlc -c $$mls
	$$ocamlopt -c $$mls
	$$mk_cma -g -a -o $(libname).cma $$cmos
	$$mk_cmxa -g -a -o $(libname).cmxa $$cmxs
	$(MAKE) $$natives $$bytes


_links:
	mk_links
	touch $@

_test:
	OCAMLRUNPARAM=b ./test_main.native ./default_tests.json

_generated: 
	$(MAKE) -C b_from_isa

_mlis: 
	mk_mlis

install:
	mk_meta
	install

_remove: 
	remove

_reinstall: 
	remove
	install

README.html: README.adoc
	asciidoctor $<

_depend:
	mkdir -p _depend
	mk_depend

_codetags:
	mk_codetags

_doc:
	echo Make sure to build first
	mk_doc
	cp *.html *.css ../docs

# ----------------------------------------

%.native: %.ml $(libname).cmxa FORCE
	$$ocamlopt -g -linkpkg -o $*.native $${libname}.cmxa $*.ml

%.byte: %.ml $(libname).cma FORCE
	$$ocamlc -g -linkpkg -o $*.byte $${libname}.cma $*.ml

clean:
	rm -f *.{cmi,cmo,cmx,o} a.out *.cma *.cmxa *.a *.byte *.native 

real_clean: clean
	rm_links
	rm -rf _links _depend

FORCE:


