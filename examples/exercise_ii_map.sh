#!/bin/bash

set +x

# execute various commands on a store

STORE=./btree.store
MAIN=dune exec examples/int_int_map_main.exe

rm -f $STORE
./$MAIN init $STORE
echo
./$MAIN insert $STORE 1 1
echo
./$MAIN list $STORE
echo
./$MAIN insert $STORE 2 2 
echo
./$MAIN insert $STORE 3 3 
echo
./$MAIN list $STORE

for i in `seq 4 1000`; do
    ./$MAIN insert $STORE $i $i
done

./$MAIN list $STORE
