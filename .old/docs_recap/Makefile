SHELL:=bash

all:
	echo "Please make a particular target"


# NOTE generated mli are in /tmp; assume these are the only .mli in tmp
update_orig_mli: FORCE
	for f in /tmp/*.mli; do cp $$f .; done
	for f in *.mli; do mv $$f $${f/.mli/.mli.orig}; done


# make the patch
%.patch: %.orig %.patched FORCE
	-diff -u $*.orig $*.patched > $@


# use the patch to patch up an orig
# NOTE execute this by hand
# patch -i <orig> <patch> > <patched>


FORCE:
