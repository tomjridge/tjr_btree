SHELL:=bash
BASH_ENV=bash_env.sh
export BASH_ENV


# ----------------------------------------

all: 
	for f in *.ml; do $(MAKE) $${f/.ml/.native}; done

run_examples:
	./int_int_map_example_functionality.native
	./string_string_map_example_functionality.native
	./exercise_ii_map.sh  # FIXME note startup time seems a bit long for the .native
	./exercise_ss_map.sh

test_startup_time:
	time for f in `seq 1 100`; do ./int_int_map_main.native nop; done
	echo Total time is about 1.7 s, or .017s per run

%.native: %.ml FORCE
	$$ocamlopt -g -linkpkg -o $*.native $<

clean:
	clean
	rm -f btree.store

FORCE:


