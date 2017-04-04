SHELL:=/bin/bash

FN:=isa_export

# dealing with generated code ----------------------------------------

all: $(FN).ml

# make this after editing our.ml
# https://forums.anandtech.com/threads/best-practices-for-creating-patch-with-diff.2264468/
$(FN).diff: /tmp/$(FN).ml $(FN).ml.patched
	diff -u3 -p /tmp/$(FN).ml $(FN).ml.patched >$@ 
# this fails if there are changes?

$(FN).ml: /tmp/$(FN).ml $(FN).diff 
	patch -i $(FN).diff /tmp/$(FN).ml -o $@

FORCE:
