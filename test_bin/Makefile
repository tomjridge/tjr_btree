SHELL:=bash
BASH_ENV=bash_env.sh
export BASH_ENV


# ----------------------------------------

all: 
	for f in *.ml; do $(MAKE) $${f/.ml/.native}; done

run_tests:
	./exhaustive_in_mem_main.native


%.native: %.ml FORCE
	$$ocamlopt -g -linkpkg -o $*.native $<

clean:
	clean

FORCE:


