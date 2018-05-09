#!/bin/bash

# build all dependent projects; assumes these live in ..

# NOTE this should match the relevant Dockerfile; FIXME move Dockerfile here?

DST=..

for f in isa_btree tjr_monad tjr_fs_shared path_resolution; do
    make -C $DST/$f clean
    make -C $DST/$f 
done
