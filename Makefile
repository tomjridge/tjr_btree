all:
	$(MAKE) -C src

clean:
	$(MAKE) -C src real_clean

install: 
	$(MAKE) -C src install
