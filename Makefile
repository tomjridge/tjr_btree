all:
	$(MAKE) -C src

clean:
	$(MAKE) -C src real_clean
	$(MAKE) -C examples clean
	$(MAKE) -C test_bin clean

install: 
	$(MAKE) -C src install
