SHELL:=/bin/bash
BASH_ENV=../bash_env.sh
export BASH_ENV


FN:=isa_export

# dealing with generated code ----------------------------------------

all: $(FN).ml

# make this after editing our.ml
# https://forums.anandtech.com/threads/best-practices-for-creating-patch-with-diff.2264468/
$(FN).diff: FORCE
	-diff -u3 -p $(FN).ml_ $(FN).ml >$@

$(FN).ml: $(FN).ml_ $(FN).diff
	patch -i $(FN).diff $(FN).ml_ -o $@

FORCE:
