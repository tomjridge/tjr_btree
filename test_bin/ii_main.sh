#!/bin/bash

set +x

# execute various commands on a store

STORE=./btree.store
MAIN=main.native
x=ii

rm -f $STORE
./$MAIN $x init $STORE
echo
./$MAIN $x insert $STORE 1 1
echo
./$MAIN $x list $STORE
echo
./$MAIN $x insert $STORE 2 2 
echo
./$MAIN $x insert $STORE 3 3 
echo
./$MAIN $x list $STORE

for i in `seq 4 1000`; do
    ./$MAIN $x insert $STORE $i $i
done

./$MAIN $x list $STORE
