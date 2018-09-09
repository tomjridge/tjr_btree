NAME:=tjr_btree

from_github: 
	docker build -t $(NAME)_from_github -f Dockerfile.from_github .

using_opam:
	docker build -t $(NAME)_using_opam -f Dockerfile.using_opam .


# 	rm -rf tjr_lib
# 	$(MAKE) -C ../src clean
# 	mkdir tjr_lib
# 	rsync -vaz ../src ./tjr_lib
# 	rsync -vaz ../VERSION ../Makefile ./tjr_lib
# 	chmod -R ugo+rw tjr_lib
# 	docker build -t $(NAME)_local -f Dockerfile.local .

#	docker run -ti $NAME bash

clean:
#rm -rf tjr_lib

