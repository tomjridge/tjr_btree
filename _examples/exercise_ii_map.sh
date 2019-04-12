#!/bin/bash

echo "exercise_ii_map.sh ----------------------------------------------"

set +x

# execute various commands on a store

STORE=./btree.store
MAIN=int_int_map_main
echo

echo Executing various simple operations -------------------------------
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
echo


echo Start inserting many entries... -----------------------------------
echo NOTE this would be faster with insert_many

# NOTE this is slow because of repeated init time etc.
# echo Start inserting many entries
# for i in `seq 4 1000`; do
#     $MAIN insert $STORE $i $i
# done
# echo Finish

time $MAIN insert_range $STORE 4 10000
echo Finish 
echo NOTE should take about .6s for 10k entries, with some of that as startup overhead
echo

# echo Listing entries ---------------------------------------------------
# $MAIN list $STORE
