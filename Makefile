DUNE:=dune

build:
	$(DUNE) build @install

install:
	$(DUNE) install

uninstall:
	$(DUNE) uninstall

clean:
	$(DUNE) clean

doc: FORCE
	$(DUNE) build @doc

doc_install: doc
	rm -rf ocamldoc/*
	cp -R _build/default/_doc/_html/* ocamldoc

FORCE:


# 
# clean:
# 	$(MAKE) -C src real_clean
# 	$(MAKE) -C examples clean
# 	$(MAKE) -C test_bin clean

