all:
	$(MAKE) -C src

clean:
	$(MAKE) -C src clean
	$(MAKE) -C examples clean
	$(MAKE) -C test_bin clean

install: 
	$(MAKE) -C src install
