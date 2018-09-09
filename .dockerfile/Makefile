NAME:=tjr_btree

all:
	$(MAKE) from_github
	$(MAKE) using_opam

from_github: 
	docker build --no-cache -t $(NAME)_from_github -f Dockerfile.from_github .

using_opam:
	docker build --no-cache -t $(NAME)_using_opam -f Dockerfile.using_opam .

test:
	docker build --no-cache -t $(NAME)_test -f Dockerfile.test .

clean:
	-docker image rm $(NAME)_from_github $(NAME)_using_opam


