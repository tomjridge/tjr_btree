#!/bin/bash

echo "exercise_ii_map.sh ----------------------------------------------"

set +x

# execute various commands on a store

STORE=./btree.store
MAIN=int_int_map_main

rm -f $STORE
$MAIN init $STORE
echo
$MAIN insert $STORE 1 1
echo
$MAIN list $STORE
echo
$MAIN insert $STORE 2 2 
echo
$MAIN insert $STORE 3 3 
echo
$MAIN list $STORE

echo Start inserting many entries
for i in `seq 4 1000`; do
    $MAIN insert $STORE $i $i
done
echo Finish

$MAIN list $STORE
