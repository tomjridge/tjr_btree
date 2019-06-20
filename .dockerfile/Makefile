NAME:=tjr_btree

all:
	$(MAKE) 

#	$(MAKE) from_github
#	$(MAKE) using_opam

from_github: 
	docker build --no-cache -t $(NAME)_from_github -f Dockerfile.from_github .

using_opam:
	docker build --no-cache -t $(NAME)_using_opam -f Dockerfile.using_opam .

test:
	docker build --no-cache -t $(NAME)_test -f Dockerfile.test .

opam_with_pinning:
	docker build --no-cache -t $(NAME)_$@ -f Dockerfile.$@ .

clean:
	-docker image rm $(NAME)_from_github $(NAME)_using_opam


