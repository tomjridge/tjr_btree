SHELL:=/bin/bash

# * Generated code
#
# After isa export, there should be a file /tmp/isa_export.ml
#
# - Copy to ./orig.ml
# - Try to apply patch: make isa_export.ml
# - If this fails, apply patch to orig, output to patched.ml and: make orig.patch
#
# https://forums.anandtech.com/threads/best-practices-for-creating-patch-with-diff.2264468/

all: 
	echo "Need to make a particular target"
	false

orig.patch: orig.ml patched.ml
	diff -u3 -p orig.ml patched.ml >$@ 
# this fails if there are changes?

isa_export.ml: orig.ml FORCE
	-patch -i orig.patch orig.ml -o $@ 
	cp $@ ../ac_test

FORCE:
