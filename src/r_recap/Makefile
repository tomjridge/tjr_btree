SHELL:=bash

all:
	for f in *.ml; do $(MAKE) $${f/.ml/.mli}; done

# NOTE result is in /tmp
%.mli: %.ml FORCE
	echo "NOTE you have to have built and installed tjr_btree before doing this; also need to reinstall to pick up changes"
	ocamlfind ocamlc -thread -package tjr_btree -i $*.ml | \
	sed \
	-e "s/Rstk[.]//g" \
	-e "s/Pre_map_ops[.]pre_map_ops/pre_map_ops/g" \
	-e "s/Base_types[.]//g" \
	-e "s/Block[.]//g" \
	-e "s/Frame[.]//g" \
	-e "s/Monad[.]mref/mref/g" \
	-e "s/Monad[.]m/m/g" \
	-e "s/Isa_export[.]Tree[.]//g" \
	-e "s/Tree[.]//g" \
	-e "s/Store_ops[.]store_ops/store_ops/g" \
	-e "s/Page_ref_int[.]page_ref/page_ref_int/g" \
	-e "s/Tjr_btree[.]//g" >/tmp/$*.mli

FORCE:
